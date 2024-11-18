use crossterm::event::{DisableMouseCapture, EnableMouseCapture};
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use futures::StreamExt;
use ratatui::backend::CrosstermBackend;
use ratatui::style::{Color, Modifier, Style};
use ratatui::widgets::{Block, Borders};
use ratatui::Terminal;
use std::env;
use std::fmt;
use std::fs;
use std::io;
use std::io::BufRead;
use tui_textarea::{CursorMove, Input, Key, Scrolling, TextArea};

// Audio help-- for ami
use std::fmt::Display;
use cpal::traits::{HostTrait, DeviceTrait, StreamTrait};
use cpal::{Sample, FromSample, SizedSample};
#[cfg(feature = "audio_log")]
use std::io::Write; // Later we might want this for file writing.
#[cfg(feature = "audio_log")]
type AudioLog = std::fs::File;
#[cfg(not(feature = "audio_log"))]
type AudioLog = ();
#[derive(Debug)]
enum CpalError {
    Build(cpal::BuildStreamError),
    Play(cpal::PlayStreamError),
    NoDevice,
    Unknown
}
impl std::error::Error for CpalError {}
impl Display for CpalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl From<cpal::BuildStreamError> for CpalError { fn from(e: cpal::BuildStreamError) -> Self { CpalError::Build(e) } }
impl From<cpal::PlayStreamError> for CpalError { fn from(e: cpal::PlayStreamError) -> Self { CpalError::Play(e) } }
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
// End audio help

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Normal,
    Insert,
    Replace(bool), // Bool for "once only?"
    Visual,
    Operator(char),
}

impl Mode {
    fn block<'a>(&self) -> Block<'a> {
        let help = match self {
            Self::Normal => "type q to quit, type i to enter insert mode",
            Self::Replace(_) | Self::Insert => "type Esc to back to normal mode",
            Self::Visual => "type y to yank, type d to delete, type Esc to back to normal mode",
            Self::Operator(_) => "move cursor to apply operator",
        };
        let title = format!("{} MODE ({})", self, help);
        Block::default().borders(Borders::ALL).title(title)
    }

    fn cursor_style(&self) -> Style {
        let color = match self {
            Self::Normal => Color::Reset,
            Self::Insert => Color::LightBlue,
            Self::Visual => Color::LightYellow,
            Self::Replace(_) => Color::LightRed,
            Self::Operator(_) => Color::LightGreen,
        };
        Style::default().fg(color).add_modifier(Modifier::REVERSED)
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Normal => write!(f, "NORMAL"),
            Self::Insert => write!(f, "INSERT"),
            Self::Visual => write!(f, "VISUAL"),
            Self::Replace(_) => write!(f, "REPLACE"),
            Self::Operator(c) => write!(f, "OPERATOR({})", c),
        }
    }
}

// How the Vim emulation state transitions
enum Transition {
    Nop,
    Mode(Mode),
    Pending(Input),
    Quit,
}

struct VimAudioSeed {
    time: std::sync::Arc<AtomicU32>,
    play: std::sync::Arc<AtomicBool>
}

// State of Vim emulation
struct Vim {
    mode: Mode,
    pending: Input, // Pending input to handle a sequence with two keys like gg
    audio:VimAudioSeed
}

impl Vim {
    fn new(mode: Mode, audio:VimAudioSeed) -> Self {
        Self {
            mode,
            pending: Input::default(),
            audio
        }
    }

    fn with_pending(self, pending: Input) -> Self {
        Self {
            mode: self.mode,
            pending,
            audio: self.audio
        }
    }

    // True if the textarea cursor is at the end of its given line
    fn is_before_line_end(textarea: &TextArea<'_>) -> bool {
        let (cursor_line, cursor_char) = textarea.cursor();
        let lines = textarea.lines();
        let line = &lines[cursor_line];
        let line_length = line.chars().count(); // FIXME: Not acceptable-- O(N)

        cursor_char < line_length
    }

    fn transition(&self, input: Input, textarea: &mut TextArea<'_>) -> Transition {
        if input.key == Key::Null {
            return Transition::Nop;
        }

        match self.mode {
            Mode::Normal | Mode::Visual | Mode::Operator(_) => {
                match input {
                    Input {
                        key: Key::Char('h'),
                        ..
                    } |
                    Input {
                        key: Key::Left,
                        ..
                    } => textarea.move_cursor(CursorMove::Back),

                    Input {
                        key: Key::Char('j'),
                        ..
                    } |
                    Input {
                        key: Key::Down,
                        ..
                    } => textarea.move_cursor(CursorMove::Down),

                    Input {
                        key: Key::Char('k'),
                        ..
                    } |
                    Input {
                        key: Key::Up,
                        ..
                    } => textarea.move_cursor(CursorMove::Up),

                    Input {
                        key: Key::Char('l'),
                        ..
                    } |
                    Input {
                        key: Key::Right,
                        ..
                    } => textarea.move_cursor(CursorMove::Forward),

                    Input {
                        key: Key::Char('w'),
                        ..
                    } => textarea.move_cursor(CursorMove::WordForward),
                    Input {
                        key: Key::Char('e'),
                        ctrl: false,
                        ..
                    } => {
                        textarea.move_cursor(CursorMove::WordEnd);
                        if matches!(self.mode, Mode::Operator(_)) {
                            textarea.move_cursor(CursorMove::Forward); // Include the text under the cursor
                        }
                    }
                    Input {
                        key: Key::Char('b'),
                        ctrl: false,
                        ..
                    } => textarea.move_cursor(CursorMove::WordBack),
                    Input {
                        key: Key::Char('^'),
                        ..
                    } => textarea.move_cursor(CursorMove::Head),
                    Input {
                        key: Key::Char('$'),
                        ..
                    } => textarea.move_cursor(CursorMove::End),
                    Input { // Note: Not sorted with j
                        key: Key::Char('J'),
                        ..
                    } => {
                        let cursor = textarea.cursor();
                        textarea.cancel_selection(); // FIXME: WRONG!! J when there is a selection merges the selected lines.
                        textarea.move_cursor(CursorMove::End);
                        let success = textarea.delete_line_by_end();
                        if success {
                            textarea.insert_char(' ');
                        } else { // In regular vim, joining on the final line is a noop
                            let (c1, c2) = cursor;
                            textarea.move_cursor(CursorMove::Jump(c1 as u16, c2 as u16));
                            // TODO: beep
                        }
                    }
                    Input {
                        key: Key::Char('D'),
                        ..
                    } => {
                        textarea.delete_line_by_end();
                        return Transition::Mode(Mode::Normal);
                    }
                    Input {
                        key: Key::Char('C'),
                        ..
                    } => {
                        textarea.delete_line_by_end();
                        textarea.cancel_selection();
                        return Transition::Mode(Mode::Insert);
                    }
                    Input {
                        key: Key::Char('p'),
                        ..
                    } => {
                        textarea.paste();
                        return Transition::Mode(Mode::Normal);
                    }
                    Input {
                        key: Key::Char('u'),
                        ctrl: false,
                        ..
                    } => {
                        textarea.undo();
                        return Transition::Mode(Mode::Normal);
                    }
                    Input {
                        key: Key::Char('r'),
                        ctrl: true,
                        ..
                    } => {
                        textarea.redo();
                        return Transition::Mode(Mode::Normal);
                    }
                    Input {
                        key: Key::Char('x'),
                        ..
                    } => {
                        // FIXME: This check shouldn't be necessary, but the vim example is able to cursor over a terminating newline currently, which real vim can't in normal mode
                        // FIXME: Repeatedly mashing x at the end of a line should delete the entire line right to left
                        if Vim::is_before_line_end(&textarea) {
                            textarea.delete_next_char();
                        }
                        return Transition::Mode(Mode::Normal);
                    }
                    Input {
                        key: Key::Char('i'),
                        ..
                    } => {
                        textarea.cancel_selection();
                        return Transition::Mode(Mode::Insert);
                    }
                    Input {
                        key: Key::Char('a'),
                        ..
                    } => {
                        textarea.cancel_selection();

                        if Vim::is_before_line_end(&textarea) {
                            textarea.move_cursor(CursorMove::Forward);
                        }
                        return Transition::Mode(Mode::Insert);
                    }
                    Input {
                        key: Key::Char('A'),
                        ..
                    } => {
                        textarea.cancel_selection();
                        textarea.move_cursor(CursorMove::End);
                        return Transition::Mode(Mode::Insert);
                    }
                    Input {
                        key: Key::Char('S'),
                        ..
                    } => {
                        textarea.cancel_selection(); // FIXME: WRONG!! S when there is a selection collapses and clears all lines.
                        textarea.move_cursor(CursorMove::Head);
                        let (cursor_line, _) = textarea.cursor();
                        let lines = textarea.lines();
                        let line = &lines[cursor_line];
                        if line.len() > 0 {
                            // delete_line_by_end has a special behavior where if you are at the end,
                            // it joins the line with the next. Prevent accidentally triggering this on an empty line.
                            textarea.delete_line_by_end();
                        }
                        return Transition::Mode(Mode::Insert);
                    }
                    Input {
                        key: Key::Char('o'),
                        ..
                    } => {
                        textarea.move_cursor(CursorMove::End);
                        textarea.insert_newline();
                        return Transition::Mode(Mode::Insert);
                    }
                    Input {
                        key: Key::Char('O'),
                        ..
                    } => {
                        textarea.move_cursor(CursorMove::Head);
                        textarea.insert_newline();
                        textarea.move_cursor(CursorMove::Up);
                        return Transition::Mode(Mode::Insert);
                    }
                    Input {
                        key: Key::Char('I'),
                        ..
                    } => {
                        textarea.cancel_selection();
                        textarea.move_cursor(CursorMove::Head);
                        return Transition::Mode(Mode::Insert);
                    }
                    Input {
                        key: Key::Char('r'),
                        ..
                    } => {
                        textarea.cancel_selection(); // FIXME: WRONG!! r when there is a selection replaces the selected text.
                        return Transition::Mode(Mode::Replace(true));
                    }
                    Input {
                        key: Key::Char('R'),
                        ..
                    } => {
                        textarea.cancel_selection(); // FIXME: WRONG!! R when there is a selection acts the same as S (?)
                        return Transition::Mode(Mode::Replace(false));
                    }

                    Input {
                        key: Key::Char('q'),
                        ..
                    } => return Transition::Quit,
                    Input {
                        key: Key::Char('e'),
                        ctrl: true,
                        ..
                    } => textarea.scroll((1, 0)),
                    Input {
                        key: Key::Char('y'),
                        ctrl: true,
                        ..
                    } => textarea.scroll((-1, 0)),
                    Input {
                        key: Key::Char('d'),
                        ctrl: true,
                        ..
                    } => textarea.scroll(Scrolling::HalfPageDown),
                    Input {
                        key: Key::Char('u'),
                        ctrl: true,
                        ..
                    } => textarea.scroll(Scrolling::HalfPageUp),
                    Input {
                        key: Key::Char('f'),
                        ctrl: true,
                        ..
                    } => textarea.scroll(Scrolling::PageDown),
                    Input {
                        key: Key::Char('b'),
                        ctrl: true,
                        ..
                    } => textarea.scroll(Scrolling::PageUp),
                    Input {
                        key: Key::Char('v'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Normal => {
                        textarea.start_selection();
                        return Transition::Mode(Mode::Visual);
                    }
                    Input {
                        key: Key::Char('V'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Normal => {
                        textarea.move_cursor(CursorMove::Head);
                        textarea.start_selection();
                        textarea.move_cursor(CursorMove::End);
                        return Transition::Mode(Mode::Visual);
                    }
                    Input { key: Key::Esc, .. }
                    | Input {
                        key: Key::Char('v'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.cancel_selection();
                        return Transition::Mode(Mode::Normal);
                    }
                    Input {
                        key: Key::Char('g'),
                        ctrl: false,
                        ..
                    } if matches!(
                        self.pending,
                        Input {
                            key: Key::Char('g'),
                            ctrl: false,
                            ..
                        }
                    ) =>
                    {
                        textarea.move_cursor(CursorMove::Top)
                    }
                    Input {
                        key: Key::Char('G'),
                        ctrl: false,
                        ..
                    } => textarea.move_cursor(CursorMove::Bottom),
                    Input {
                        key: Key::Char(c),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Operator(c) => {
                        // Handle yy, dd, cc. (This is not strictly the same behavior as Vim)
                        textarea.move_cursor(CursorMove::Head);
                        textarea.start_selection();
                        let cursor = textarea.cursor();
                        textarea.move_cursor(CursorMove::Down);
                        if cursor == textarea.cursor() {
                            textarea.move_cursor(CursorMove::End); // At the last line, move to end of the line instead
                        }
                    }
                    Input {
                        key: Key::Char(op @ ('y' | 'd' | 'c')),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Normal => {
                        textarea.start_selection();
                        return Transition::Mode(Mode::Operator(op));
                    }
                    Input {
                        key: Key::Char('y'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.move_cursor(CursorMove::Forward); // Vim's text selection is inclusive
                        textarea.copy();
                        return Transition::Mode(Mode::Normal);
                    }
                    Input {
                        key: Key::Char('d'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.move_cursor(CursorMove::Forward); // Vim's text selection is inclusive
                        textarea.cut();
                        return Transition::Mode(Mode::Normal);
                    }
                    Input {
                        key: Key::Char('c'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.move_cursor(CursorMove::Forward); // Vim's text selection is inclusive
                        textarea.cut();
                        return Transition::Mode(Mode::Insert);
                    }
                    input => return Transition::Pending(input),
                }

                // Handle the pending operator
                match self.mode {
                    Mode::Operator('y') => {
                        textarea.copy();
                        Transition::Mode(Mode::Normal)
                    }
                    Mode::Operator('d') => {
                        textarea.cut();
                        Transition::Mode(Mode::Normal)
                    }
                    Mode::Operator('c') => {
                        textarea.cut();
                        Transition::Mode(Mode::Insert)
                    }
                    _ => Transition::Nop,
                }
            }
            Mode::Insert => match input {
                Input { key: Key::Esc, .. }
                | Input {
                    key: Key::Char('c'),
                    ctrl: true,
                    ..
                } => Transition::Mode(Mode::Normal),
                input => {
                    textarea.input(input); // Use default key mappings in insert mode
                    Transition::Mode(Mode::Insert)
                }
            },
            Mode::Replace(once) => match input {
                Input { key: Key::Esc, .. }
                | Input {
                    key: Key::Char('c'),
                    ctrl: true,
                    ..
                } => Transition::Mode(Mode::Normal),
                Input { key, .. }  => {
                    if match key { Key::Down | Key::Up | Key::Left | Key::Right => false, _ => true }
                    && Vim::is_before_line_end(&textarea) {
                        textarea.delete_next_char(); // FIXME: Will eat newlines and join into next line, should act like insert at end of line 
                    }
                    textarea.input(input); // Use default key mappings in insert mode
                    if once {
                        Transition::Mode(Mode::Normal)   
                    } else {
                        Transition::Mode(Mode::Replace(false))
                    }
                }
            }
        }
    }
}

struct AudioSeed {
    time: std::sync::Arc<AtomicU32>,
    play: std::sync::Arc<AtomicBool>
}

fn audio_write<T>(output: &mut [T], channels: usize, next_sample: &mut dyn FnMut() -> f32, audio_log: &mut AudioLog)
where
    T: Sample + FromSample<f32> + bytemuck::Pod, /* Pod constraint can be removed without audio_log */
{
    // Chop output array into slices of size "channels"
    for frame in output.chunks_mut(channels) {
        let value: T = T::from_sample(next_sample());

        // Take one sample and interleave it into all channels
        for sample in frame.iter_mut() {
            *sample = value;
        }

        #[cfg(feature = "audio_log")]
        {
            audio_log.write_all(bytemuck::cast_slice(&[value]));
        }
    }
}

fn audio_run<T>(device: &cpal::Device, config: &cpal::StreamConfig, audio_additional:AudioSeed) -> Result<cpal::Stream, CpalError>
where
    T: SizedSample + FromSample<f32> + bytemuck::Pod, /* Pod constraint can be removed without audio_log */
{
//    let sample_rate = config.sample_rate.0 as f32;
    let channels = config.channels as usize;
    let mut counter = 0;

    let audio_time = audio_additional.time.clone();
    let audio_playing = audio_additional.play.clone();

    let mut next_value = move || {
        counter += 1;
        let (is_b0, is_b1, is_b2) = (0 != counter & (1 << 14), 0 != counter & (1 << 15), 0 != counter & (1 << 16));

        let b0 = if is_b0 {  1 } else { 0 };
        let b1 = if is_b1 { -1 } else { 0 };
        let b2 = if is_b2 { -2 } else { 0 };

        let b0i = if is_b0 {  1 } else { 0 };
        let b1i = if is_b1 {  2 } else { 0 };
        let b2i = if is_b2 {  4 } else { 0 };

        audio_time.store(b0i + b1i + b2i, Ordering::Relaxed);

        if 0 != counter & (1<<(7 + b0 + b1 + b2)) {
            0.25
        } else {
            0.0
        }
        // -- BOILERPLATE --
    };

    let mut zero_value = || { 0.0 };

    let err_fn = |err| panic!("an error occurred on stream: {}", err); // FIXME: Geez don't panic

    #[cfg(feature = "audio_log")]
    let mut audio_log = std::fs::File::create("audio_log.raw").unwrap();
    #[cfg(not(feature = "audio_log"))]
    let mut audio_log:AudioLog = ();

    let stream = device.build_output_stream(
        config,
        move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
            audio_write(data, channels,
                if audio_playing.load(Ordering::Relaxed) {
                    &mut next_value
                } else {
                    &mut zero_value
                },
            &mut audio_log)
        },
        err_fn,
        None,
    )?;
    stream.play()?;

    Ok(stream)
}


fn ami_boot_audio(audio_additional:AudioSeed) -> Option<cpal::Stream> {
    let host = cpal::default_host();
    if let Some(device) = host.default_output_device() {
        let config = device.default_output_config().unwrap();

        let stream_result = match config.sample_format() {
            cpal::SampleFormat::I8 => audio_run::<i8>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::I16 => audio_run::<i16>(&device, &config.into(), audio_additional),
            // cpal::SampleFormat::I24 => audio_run::<I24>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::I32 => audio_run::<i32>(&device, &config.into(), audio_additional),
            // cpal::SampleFormat::I48 => audio_run::<I48>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::I64 => audio_run::<i64>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::U8 => audio_run::<u8>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::U16 => audio_run::<u16>(&device, &config.into(), audio_additional),
            // cpal::SampleFormat::U24 => audio_run::<U24>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::U32 => audio_run::<u32>(&device, &config.into(), audio_additional),
            // cpal::SampleFormat::U48 => audio_run::<U48>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::U64 => audio_run::<u64>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::F32 => audio_run::<f32>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::F64 => audio_run::<f64>(&device, &config.into(), audio_additional),
            sample_format => panic!("Unsupported sample format '{sample_format}'"),
        };

        match stream_result {
            Err(e) => {
                //warn!("Audio startup failure: {}", e); // TODO : logging
                None
            },
            Ok(v) => {
                //trace!("Audio startup success");
                Some(v)
            }
        }
    } else {
        panic!("Failure: No audio device");
        None
    }
}

#[tokio::main]
async fn main() -> io::Result<()> {
    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    let audio_time = std::sync::Arc::new(AtomicU32::new(0));
    let audio_play = std::sync::Arc::new(AtomicBool::new(true));
    let audio_seed = AudioSeed { time: audio_time.clone(), play: audio_play.clone() };
    let audio = ami_boot_audio(audio_seed);

    enable_raw_mode()?;
    crossterm::execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut term = Terminal::new(backend)?;

    let mut textarea = if let Some(path) = env::args().nth(1) {
        let file = fs::File::open(path)?;
        io::BufReader::new(file)
            .lines()
            .collect::<io::Result<_>>()?
    } else {
        TextArea::default()
    };

    textarea.set_block(Mode::Normal.block());
    textarea.set_cursor_style(Mode::Normal.cursor_style());
    let mut vim = Vim::new(Mode::Normal, VimAudioSeed { time:audio_time, play:audio_play.clone() }); // Note: time NOT cloned

    const FRAMES_PER_SECOND:f32 = 60.0;
    let period = std::time::Duration::from_secs_f32(1.0 / FRAMES_PER_SECOND);
    let mut interval = tokio::time::interval(period);
    let mut events = crossterm::event::EventStream::new();
    let mut should_quit = false;

    while !should_quit {
        tokio::select! {
            // FIXME: rather than this wait on mspc messages or something
            // Used to this happened every loop
            _ = interval.tick() => { term.draw(|f| {
                    f.render_widget(&textarea, f.area());

                    let bar = ratatui::widgets::Paragraph::new(format!("{} {}", if (*vim.audio.play).load(Ordering::Relaxed) { "PLAYING" } else {"Paused "}, (*vim.audio.time).load(Ordering::Relaxed)));
                    let mut area = f.area();
                    area.y = area.height-1;
                    area.height=1;
                    f.render_widget(bar, area);
                })?;
            },

            // Terminal event
            Some(Ok(event)) = events.next() => {
                match event.clone().into() { // Mode-indifferent overrides
                    Input {
                        key: Key::Char('p'),
                        ctrl: true,
                        ..
                    } => {
                        audio_play.fetch_xor(true, Ordering::Relaxed);
                    },
                    _ => { // Mode match
                        vim = match vim.transition(event.into(), &mut textarea) {
                            Transition::Mode(mode) if vim.mode != mode => {
                                textarea.set_block(mode.block());
                                textarea.set_cursor_style(mode.cursor_style());
                                Vim::new(mode, vim.audio)
                            }
                            Transition::Nop | Transition::Mode(_) => vim,
                            Transition::Pending(input) => vim.with_pending(input),
                            Transition::Quit => { should_quit = true; vim },
                        }
                    }
                }
            },
        }
    }

    disable_raw_mode()?;
    crossterm::execute!(
        term.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    term.show_cursor()?;

    println!("Lines: {:?}", textarea.lines());

    Ok(())
}
