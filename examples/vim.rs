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
use atomicbox::AtomicOptionBox;
// End audio help

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Normal,
    Insert,
    Replace(bool), // Bool for "once only?"
    Visual,
    Command, // "colon mode", what the manual calls Command-line mode
    Operator(char),
}

impl Mode {
    fn block<'a>(&self) -> Block<'a> {
        let help = match self {
            Self::Normal => "type :q to quit, type i to enter insert mode",
            Self::Replace(_) | Self::Insert => "type Esc to back to normal mode",
            Self::Visual => "type y to yank, type d to delete, type Esc to back to normal mode",
            Self::Operator(_) => "move cursor to apply operator",
            Self::Command => "enter command at prompt, or Esc for normal mode",
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
            // FIXME: This matches behavior of vim, would it be better to underline or something?
            Self::Command => { return Style::default(); }
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
            Self::Command => write!(f, "COMMAND"),
        }
    }
}

// How the Vim emulation state transitions
#[derive(Clone)]
enum Transition {
    Nop,
    Mode(Mode),
    Pending(Input),
    Quit,
}

// For ami
struct VimAudioSeed {
    time: std::sync::Arc<AtomicU32>,
    play: std::sync::Arc<AtomicBool>,
}

// Language

// Number: Play this note
// Parenthesis: Store a pattern, first letter is the label (must be uppercase letter)
// Square bracket: Scope
// Uppercase letter: Play stored pattern
// +Number, -Number: Shift base note by semitones
// ++Number, --Number: Shift base note by octaves (or multiply/divide for non-notes)
// x: rest
// r: reset
// pNumber, p+Number, p++Number etc: Set pitch, do NOT play note
// tNumber, t+Number, t++Number etc: Set tempo (rate, high)
// dNumber, d+Number, d++Number etc: set duty/duration (against basis of 8)
// a&b: One, then the other

// TODOs/document: pv, dv; !
// TODOs/consider: pr, dr; &&, &&&; !!, !; :;

// In comments below: An //AT comment implies audio thread only, a //PT comment implies processing thread only
// There are two forms of this AST, a "raw" form and a "clean" form, but they have the same type.

enum Act {
    Set(i32),
    Increment(i32),
    Double(i32),
    Versus(i32)
}

enum Adjust {
    Pitch(Act, bool), // PT // Argument 2 is for "hard/send", e.g., p+ vs p++
    Tempo(Act),
    Duty(Act),
    Reset // FIXME: Consider partials?
}

// The "primary value" of a note. May or may not literally correspond to pitch.
// I tried really hard to think of a name for this enum other than "pitch" and failed.
enum Pitch {
    Abs(i32), // AT (0 == rest); MUST be sanitized for 0-128 range by time reaches AT
    Rel(i32), // PT
    Rest      // PT
}

type Note = (Vec<Adjust>, Pitch); // No adjustments -> vec len 0

enum Node {
    Play(Note),
    Fork(Vec<Node>)
}

#[derive(Default)]
struct Song {
    prefix: Vec<Adjust>, // TODO consider tinyvec
    score: Vec<Node>
}

// - TODO: Do the "name" args serve a purpose? Do they create perf issues? Should they be commented out?
fn parse_language(input:String) -> Result<Song, pom::Error> { // FIXME: &String?
    use pom::utf8::*;

    fn opt_space<'a>() -> Parser<'a, ()> {
        one_of(" \t\r\n").repeat(0..).discard()
            .name("opt_space")
    }

    fn space<'a>() -> Parser<'a, ()> {
        one_of(" \t\r\n").repeat(1..).discard()
            .name("space")
    }

    fn positive<'a>() -> Parser<'a, i32> {
        let integer = (one_of("123456789").discard() * one_of("0123456789").discard().repeat(0..)).discard()
            | sym('0').discard();
//      sym('-').discard().opt() * // TODO: negative numbers via quotes
//      let integer = digit.discard().repeat(1..);
        integer.collect().convert(|x| x.parse::<i32>())
            .name("positive")
    }

    fn act<'a>(set:bool) -> Parser<'a, Act> {
        {
            let mut p = (sym('v') * positive()).map(|x| Act::Versus(x))
            | (seq("++") * positive()).map(|x| Act::Double(x))
            | (seq("--") * positive()).map(|x| Act::Double(-x))
            | (sym('+') * positive()).map(|x| Act::Increment(x))
            | (sym('-') * positive()).map(|x| Act::Increment(-x));
            if set {
                p = p | positive().map(|x| Act::Set(x))
            }
            p
        }.name("act")
    }

    fn adjust<'a>() -> Parser<'a, Adjust> {
        (
            sym('r').map(|_|Adjust::Reset)
            | (sym('d') * act(true)).map(|act|Adjust::Duty(act))
            | (sym('t') * act(true)).map(|act|Adjust::Tempo(act))
            | (sym('p') * positive()).map(|x|Adjust::Pitch(Act::Set(x), true))
            | (sym('p').opt() + act(false)).map(|(hard, act)|Adjust::Pitch(act, hard.is_some()))
        ).name("adjust")
    }

    fn note_pitch<'a>() -> Parser<'a, Pitch> {
        (
            sym('x').map(|_|Pitch::Rest) | positive().map(Pitch::Rel)
        ).name("pitch")
    }

    fn note<'a>() -> Parser<'a, Note> { // TODO: Collapse Option<Vec<Adjust>> into Vec<Adjust> and use 0..?
        (
            (adjust() - space()).repeat(0..) + note_pitch()
        ).name("note")
    }

    // Utility
    fn tuple_merge<T>(t:(T, Vec<T>)) -> Vec<T> {
        let (val, vec) = t;
        let mut result = vec![val];
        result.extend(vec);
        result
    }

    fn node<'a>() -> Parser<'a, Node> {
        (
            (
                note().map(Node::Play) + (opt_space() * sym('&') * opt_space() * note().map(Node::Play)).repeat(1..)
            ).map(tuple_merge).map(Node::Fork)
            | note().map(Node::Play)
        ).name("node")
    }

//    let upper = one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ");

    // TODO: parser should produce a song
    let parser =
        (
            opt_space() * sym('!') * opt_space() *
            (adjust() + (one_of(" \t") * adjust()).repeat(0..)).map(tuple_merge)
            - sym('\n')
        ).repeat(0..).map(|v|v.into_iter().flatten().collect()) +
        (
            opt_space() *
            (node() + (space() * node()).repeat(0..)).map(tuple_merge) - opt_space() - end()
        )
    ;
    parser.map(|(prefix, score)|Song {prefix, score}).parse_str(&input)
}

// Translate PT to AT
fn absolute_language(s:Song) -> Song {
    fn fit_range(x:i32) -> i32 {
        x.clamp(0,127)
    }
    fn absolute_node(node:Node) -> Node {
        match node {
            Node::Play((adjust, pitch)) => {
                Node::Play((adjust, match pitch {
                    Pitch::Rest => Pitch::Abs(0),
                    Pitch::Rel(x) => Pitch::Abs(fit_range(x + 69 - 12)), // TODO: Remove -12 when it's easier to shift octave
                    Pitch::Abs(x) => Pitch::Abs(fit_range(x))
                }))
            },
            Node::Fork(all) => Node::Fork(all.into_iter().map(absolute_node).collect())
        }
    }

    // Todo: Surf prefix for note offsets
    Song{prefix:s.prefix, score:s.score.into_iter().map(absolute_node).collect()}
}

// Command line parser

enum CommandLineTotality {
    Auto,
    All,
    Buffer,
}

enum CommandLineSetType {
    On,
    Off,
    Question
}

// TODO: reset, reset!, set for tones, play
enum CommandLine {
    Wqae(bool, bool, CommandLineTotality, bool), // Write, Quit, All/Buffer, Exclamation
    Help(String),
    Set(String, CommandLineSetType), // What you set, what you set it to // TODO: Local?
    Beep
//    Play(Option<u32>)
//    Split(Option<String>), // Filename
}

// TODO: ignore surrounding whitespace
fn parse_command_line(input:String) -> Result<CommandLine, pom::Error> { // FIXME: &String?
    use pom::utf8::*;

    fn space<'a>() -> Parser<'a, ()> {
        one_of(" \t").repeat(1..).discard()
    }

    fn unspace<'a>() -> Parser<'a, ()> {
        none_of(" \t").repeat(1..).discard()
    }

    // wqa! . Gets its own breakout cuz it's complicated
    let wqae = (
            ( // one of w or q must be present
                seq("q").map(|_|(false, true)) // q by itself
                | (                            // w or wq
                    seq("w").map(|_|(true)) +
                    seq("q").opt().map(|x|x.is_some())
                  )
            ) + ( // a/all, or b/buffer?
                (seq("a") * seq("ll").opt() ).map(|_|CommandLineTotality::All)
                | (seq("b") * seq("uffer").opt() ).map(|_|CommandLineTotality::Buffer)
            ).opt().map(|x|x.unwrap_or(CommandLineTotality::Auto))
            + seq("!").opt().map(|x|x.is_some()) // Force?
        ).map(|(((a,b),c),d)| CommandLine::Wqae(a,b,c,d));

    let parser = (
        wqae

        | (seq("help") * space() * unspace().collect().map(|x| CommandLine::Help(x.to_string())))

        | (seq("set") * space()) * (
                unspace().collect() +
                (space() * (
                    seq("0").map(|_|CommandLineSetType::Off)
                    | seq("1").map(|_|CommandLineSetType::On)
                    | seq("?").map(|_|CommandLineSetType::Question)
                ))
            ).map(|(s,t)|CommandLine::Set(s.to_string(), t))

        | (seq("beep").map(|_| CommandLine::Beep))
    ) - end();
    parser.parse_str(&input)
}

// State of Vim emulation
struct Vim {
    mode: Mode,
    pending: Input, // Pending input to handle a sequence with two keys like gg
    dirty: bool,
    audio:VimAudioSeed
}

impl Vim {
    fn new(mode: Mode, audio:VimAudioSeed, dirty:bool) -> Self {
        Self {
            mode,
            pending: Input::default(),
            dirty,
            audio
        }
    }

    fn beep(&self) { // TODO: offer CPAL, blink options? // Note: Takes self but doesn't use it (yet?)
        print!("\x07");
    }

    fn with_pending(self, pending: Input) -> Self {
        Self {
            mode: self.mode,
            pending,
            dirty: self.dirty,
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

    // Result: Next state, dirtied this interaction?
    fn transition(&self, input: Input, textarea: &mut TextArea<'_>, command: &mut TextArea<'_>) -> (Transition, bool) {
        if input.key == Key::Null {
            return (Transition::Nop, false);
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
                        let mut cursor = textarea.cursor();
                        let mut line_count = 1;
                        let mut dirty = false;

                        if let Some(((from_line, from_idx), (to_line, _))) = textarea.selection_range() {
                            // J with a selection joins all lines selected
                            // If only one line is selected, it acts like normal J,
                            // except on failure the cursor moves to selection start.
                            line_count = (to_line-from_line).max(1);
                            cursor = (from_line, from_idx);
                            textarea.cancel_selection(); // fixme restore
                            textarea.move_cursor(CursorMove::Jump(from_line as u16, from_idx as u16));
                        }

                        for _ in 0..line_count {
                            textarea.move_cursor(CursorMove::End);
                            let success = textarea.delete_line_by_end();
                            if success { // A line existed
                                textarea.insert_char(' ');
                                dirty = true;
                            } else { // In regular vim, joining on the final line is a noop
                                let (c1, c2) = cursor;
                                textarea.move_cursor(CursorMove::Jump(c1 as u16, c2 as u16));
                                self.beep();
                            }
                        }
                        return (Transition::Nop, dirty);
                    }
                    Input {
                        key: Key::Char('D'),
                        ..
                    } => {
                        textarea.delete_line_by_end();
                        return (Transition::Mode(Mode::Normal), true);
                    }
                    Input {
                        key: Key::Char('C'),
                        ..
                    } => {
                        textarea.delete_line_by_end();
                        textarea.cancel_selection();
                        return (Transition::Mode(Mode::Insert), true);
                    }
                    Input {
                        key: Key::Char('p'),
                        ..
                    } => {
                        textarea.paste();
                        return (Transition::Mode(Mode::Normal), true);
                    }
                    Input {
                        key: Key::Char('u'),
                        ctrl: false,
                        ..
                    } => {
                        textarea.undo();
                        return (Transition::Mode(Mode::Normal), true);
                    }
                    Input {
                        key: Key::Char('r'),
                        ctrl: true,
                        ..
                    } => {
                        textarea.redo();
                        return (Transition::Mode(Mode::Normal), true);
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
                        return (Transition::Mode(Mode::Normal), true);
                    }
                    Input {
                        key: Key::Char('i'),
                        ..
                    } => {
                        textarea.cancel_selection();
                        return (Transition::Mode(Mode::Insert), false);
                    }
                    Input {
                        key: Key::Char('a'),
                        ..
                    } => {
                        textarea.cancel_selection();

                        if Vim::is_before_line_end(&textarea) {
                            textarea.move_cursor(CursorMove::Forward);
                        }
                        return (Transition::Mode(Mode::Insert), false);
                    }
                    Input {
                        key: Key::Char('A'),
                        ..
                    } => {
                        textarea.cancel_selection();
                        textarea.move_cursor(CursorMove::End);
                        return (Transition::Mode(Mode::Insert), false);
                    }
                    Input {
                        key: Key::Char('S'),
                        ..
                    } => {
                        let mut line_count = 1;

                        if let Some(((from_line, from_idx), (to_line, _))) = textarea.selection_range() {
                            // S with a selection clears all lines selected
                            line_count = (to_line-from_line).max(1);

                            textarea.cancel_selection(); // fixme restore
                            textarea.move_cursor(CursorMove::Jump(from_line as u16, from_idx as u16));
                        }

                        textarea.move_cursor(CursorMove::Head);
                        for line_idx in 0..line_count {
                            let (cursor_line, _) = textarea.cursor();
                            let lines = textarea.lines();
                            let line = &lines[cursor_line];

                            if line.len() > 0 {
                                // delete_line_by_end has a special behavior where if you are at the end,
                                // it joins the line with the next. Prevent accidentally triggering this on an empty line.
                                textarea.delete_line_by_end();
                            }

                            if line_idx < line_count-1 {
                                // We are now guaranteed at the end of the line.
                                // Join to next line.
                                textarea.delete_line_by_end();
                            }
                        }

                        return (Transition::Mode(Mode::Insert), true);
                    }
                    Input {
                        key: Key::Char('o'),
                        ..
                    } => {
                        textarea.move_cursor(CursorMove::End);
                        textarea.insert_newline();
                        return (Transition::Mode(Mode::Insert), true);
                    }
                    Input {
                        key: Key::Char('O'),
                        ..
                    } => {
                        textarea.move_cursor(CursorMove::Head);
                        textarea.insert_newline();
                        textarea.move_cursor(CursorMove::Up);
                        return (Transition::Mode(Mode::Insert), true);
                    }
                    Input {
                        key: Key::Char('I'),
                        ..
                    } => {
                        textarea.cancel_selection();
                        textarea.move_cursor(CursorMove::Head);
                        return (Transition::Mode(Mode::Insert), false);
                    }
                    Input {
                        key: Key::Char('r'),
                        ..
                    } => {
                        // Notice selection is not cancelled-- it will be used by replace mode
                        return (Transition::Mode(Mode::Replace(true)), false);
                    }
                    Input {
                        key: Key::Char('R'),
                        ..
                    } => {
                        if textarea.selection_range().is_some() {
                            // R with a selection does the same thing as S-- it enters Insert NOT Replace mode.
                            return self.transition(Input { key: Key::Char('S'), ctrl: false, alt: false, shift: true }, textarea, command);
                        } else {
                            return (Transition::Mode(Mode::Replace(false)), false);
                        }
                    }

                    /*
                    // You're not getting out so easily
                    Input {
                        key: Key::Char('q'),
                        ..
                    } => return Transition::Quit,
                    */
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
                    } => textarea.scroll(Scrolling::PageDown), // FIXME but don't scroll below end
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
                        return (Transition::Mode(Mode::Visual), false);
                    }
                    Input {
                        key: Key::Char('V'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Normal => {
                        textarea.move_cursor(CursorMove::Head);
                        textarea.start_selection();
                        textarea.move_cursor(CursorMove::End);
                        return (Transition::Mode(Mode::Visual), false);
                    }
                    Input { key: Key::Esc, .. }
                    | Input {
                        key: Key::Char('['),
                        ctrl: true,
                        ..
                    }
                    | Input {
                        key: Key::Char('v'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.cancel_selection();
                        return (Transition::Mode(Mode::Normal), false);
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
                        return (Transition::Mode(Mode::Operator(op)), false);
                    }
                    Input {
                        key: Key::Char('y'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.move_cursor(CursorMove::Forward); // Vim's text selection is inclusive
                        textarea.copy();
                        return (Transition::Mode(Mode::Normal), false);
                    }
                    Input {
                        key: Key::Char('d'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.move_cursor(CursorMove::Forward); // Vim's text selection is inclusive
                        textarea.cut();
                        return (Transition::Mode(Mode::Normal), true);
                    }
                    Input {
                        key: Key::Char('c'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.move_cursor(CursorMove::Forward); // Vim's text selection is inclusive
                        textarea.cut();
                        return (Transition::Mode(Mode::Insert), true);
                    }
                    Input {
                        key: Key::Char(':'),
                        ..
                    } => {
                        // Notice selection is not canceled
                        // TODO: Factor out
                        command.cancel_selection();
                        command.move_cursor(CursorMove::Jump(0,0));
                        while command.delete_line_by_end() {} // Erase until it fails to erase

                        return (Transition::Mode(Mode::Command), false);
                    }
                    input => return (Transition::Pending(input), false),
                }

                // Handle the pending operator
                match self.mode {
                    Mode::Operator('y') => {
                        textarea.copy();
                        (Transition::Mode(Mode::Normal), false)
                    }
                    Mode::Operator('d') => {
                        textarea.cut();
                        (Transition::Mode(Mode::Normal), true)
                    }
                    Mode::Operator('c') => {
                        textarea.cut();
                        (Transition::Mode(Mode::Insert), true)
                    }
                    _ => (Transition::Nop, false),
                }
            }
            Mode::Insert => match input {
                Input { key: Key::Esc, .. }
                | Input {
                    key: Key::Char('['),
                    ctrl: true,
                    ..
                }
                | Input {
                    key: Key::Char('c'),
                    ctrl: true,
                    ..
                } => (Transition::Mode(Mode::Normal), false),
                input => {
                    textarea.input(input); // Use default key mappings in insert mode
                    (Transition::Mode(Mode::Insert), true)
                }
            },
            Mode::Replace(once) => match input {
                Input { key: Key::Esc, .. }
                | Input {
                    key: Key::Char('['),
                    ctrl: true,
                    ..
                }
                | Input {
                    key: Key::Char('c'),
                    ctrl: true,
                    ..
                } => {
                    (if textarea.selection_range().is_some() {
                        // The user made a selection, hit lowercase 'r', then aborted.
                        // (It shouldn't be possible to get here in non-"once" mode.)
                        Transition::Mode(Mode::Visual)
                    } else {
                        Transition::Mode(Mode::Normal)
                    }, false)
                },
                Input { key, .. }  => {
                    let dirty =
                        if match key { Key::Down | Key::Up | Key::Left | Key::Right => false, _ => true }
                        && !(once && (key == Key::Backspace || key == Key::Delete)) { // Allowed with R, not r
                            if let Some(((from_line, from_idx), (to_line, to_idx))) = textarea.selection_range() {
                                // Bizarro 'r' with a selection: Replace every non-newline character at once?!
                                let mut next_start_idx = from_idx;
                                for line_idx in from_line..=to_line {
                                    let lines = textarea.lines();
                                    let line = &lines[line_idx];
                                    let line_len = line.len();

                                    textarea.move_cursor(CursorMove::Jump(line_idx as u16, next_start_idx as u16));

                                    // "min" is to handle the odd case where the cursor is on the newline (not possible in real vim)
                                    let end_idx = if line_idx==to_line { line_len.min(to_idx+1) }
                                    else { line_len };

                                    for _ in next_start_idx..end_idx {
                                        textarea.delete_next_char();
                                        textarea.input(input.clone());
                                    }

                                    next_start_idx = 0;
                                }

                                textarea.move_cursor(CursorMove::Jump(from_line as u16, from_idx as u16));
                            } else {
                                // Normal 'r'
                                if Vim::is_before_line_end(&textarea) {
                                    textarea.delete_next_char(); // FIXME: Will eat newlines and join into next line, should act like insert at end of line
                                }
                                textarea.input(input); // Use default key mappings in insert mode
                            }
                            true
                        } else {
                            self.beep();
                            false
                        };
                    (if once {
                        Transition::Mode(Mode::Normal)   
                    } else {
                        Transition::Mode(Mode::Replace(false))
                    }, dirty)
                }
            },
            Mode::Command => match input {
                // Exit command mode abnormally
                Input { key: Key::Esc, .. }
                | Input {
                    key: Key::Char('c'),
                    ctrl: true,
                    ..
                } => (Transition::Mode(Mode::Normal), false),
                // Investigate history
                // TODO scroll history
                Input { key: Key::Up, .. }
                | Input { key: Key::Down, .. } => {
                    self.beep();
                    (Transition::Mode(Mode::Command), false)
                },
                // Enter command successfully
                Input { key: Key::Enter, .. } => {
                    // Process line… this is heaviweight and maybe should be its own Thing
                    let line0 = command.lines()[0].clone();
                    let entry = parse_command_line(line0);

                    match entry { // Notice: Currently w not supported
                        Ok(CommandLine::Wqae(false, q, _totality, _exclamation)) => {
                            if q {
                                return (Transition::Quit, false); // Short circuit
                            }
                        },
                        _ => {
                            self.beep(); // TODO print useful message
                        }
                    }

                    (Transition::Mode(Mode::Normal), false)
                },
                // Type into command buffer
                _ => {
                    command.input(input);
                    (Transition::Mode(Mode::Command), false)
                }
            },
        }
    }
}

struct AudioSeed {
    time: std::sync::Arc<AtomicU32>,
    play: std::sync::Arc<AtomicBool>,
    song: std::sync::Arc<AtomicOptionBox<Song>>
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
    // Types for audio engine
    #[derive(Debug, Clone)]
    struct AdjustState {
        root:i32,        // Base note ('notes' index)
        pitch:i32,       // Pitch (index relative to base note)
        rate:i32,        // Note length (samples)
//        duty:i32,        // For what portion of the note is it "held"? (subsamples)
//        duty_vs:i32,     // Radix of duty (scalar)
//        duty_subsample_cache:i32
    }
    #[derive(Debug, Clone)]
    struct PlayState {
        // TODO Separate? Traits?
        synth_at:i32,       // Progress within square cycle (subsamples)
        synth_high:bool,    // Is square high?

        sample_at:i32,      // Progress within beat (samples)
        beat_at:usize,        // Beat ('song.score' index)
    }
    #[derive(Debug, Clone)]
    struct State { // TODO rename "frame"?
        // TODO: CoreState with pitch_vs, rate_vs?
        adjust:AdjustState,
        play:PlayState
    }

    // Constants for audio engine
    const BPM:i32 = 110;
    const SQUARE_RADIX:i32 = 32; // "Subsample" fixed point for better pitch accuracy
    const REST_NODE:Node = Node::Play((vec![], Pitch::Abs(0)));
    const DEFAULT_ADJUST:AdjustState = AdjustState {
        root:69-12, pitch:0, rate:(60.0*48000.0/(BPM as f64)/4.0) as i32,
        //duty:8, duty_vs:8
    };
    const DEFAULT_PLAY:PlayState = PlayState {
        synth_at: 0, synth_high:false, sample_at:0, beat_at:0
    };

// FIXME: sample_rate is really pretty important. Currently 44100 is assumed.
//    let sample_rate = config.sample_rate.0 as f32;
    let channels = config.channels as usize;
    let mut state = State {adjust: DEFAULT_ADJUST, play: DEFAULT_PLAY};

    // Copy cross thread values into closure
    let audio_time = audio_additional.time.clone();
    let audio_playing = audio_additional.play.clone();
    let audio_song = audio_additional.song.clone();

    // Can't have an empty song, so make a default one containing a single rest.
    let mut song:Song = Default::default();
    song.score.push(REST_NODE);

    // Lookup table of MIDI note -> subsample zero-crossing period of square wave 
    let notes = {
        let mut notes:Vec<i32> = Default::default();
        let mut base_freq = 440.0 / 32.0;
        let semitone = (2.0_f64).powf(1.0/12.0);
        for _ in 0..=8 { // I REFUSE to play notes below MIDI 9. I just DON'T WANT TO BOTHER.
            notes.push(0x7FFFFFFF);
        }
        for _x in 0..=9 { // Nine octaves above that. Whatever
            let mut freq = base_freq;
            for _y in 0..12 {
                // TODO use real sample rate
                let period = (SQUARE_RADIX as f64*44100.0_f64/freq/2.0).floor() as i32; // div two because half cycles
                //eprintln!("{}: {freq} = {period}", notes.len());
                notes.push(period);
                freq *= semitone;
            }
            base_freq *= 2.0;
        }
        notes
    };

    // Generate exactly 1 mono sample
    let mut next_value = move || {
        // Move forward sample counters
        state.play.synth_at += SQUARE_RADIX;
        state.play.sample_at += 1;

        // Check for new instructions from processing thread
        if let Some(new_song) = audio_song.swap(None, Ordering::AcqRel) {
            song = *new_song;
            if song.score.len() == 0 {
                song.score.push(REST_NODE);
            }
        }

        // Check for end of note
        if state.play.sample_at > state.adjust.rate {
            state.play.beat_at += 1;
            state.play.sample_at = 0;
            // TODO: Adjustments here
        }
        // Check for end of song (loop)
        // Do this outside previous if because song can be replaced "under us"
        if state.play.beat_at >= song.score.len() {
            state.play.beat_at = 0;
        }

        // What note are we playing?
        let pitch_index = match &song.score[state.play.beat_at] {
            Node::Play((v, Pitch::Abs(x))) if v.len()==0 => *x,
            _ => unreachable!()
        };
        // Map note->synth zero crossing peroid
        let period = notes[pitch_index as usize];

        // Time for synth zero crossing?
        if state.play.synth_at > period {
            state.play.synth_high = !state.play.synth_high;
            state.play.synth_at -= period;
        }

        // Tell the processing thread where we're at
        audio_time.store(state.play.beat_at as u32, Ordering::Relaxed);

        // Play sound
        if state.play.synth_high {
            0.25
        } else {
            0.0 // TODO: This won't work with mixing. Fix zero_value below
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
    use clap::Parser;

    #[derive(Parser)]
    struct Cli {
        #[arg(long = "play")]
        play: bool,
        filename: Option<String>
    }
    let cli = Cli::parse();

    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    let audio_time = std::sync::Arc::new(AtomicU32::new(0));
    let audio_play = std::sync::Arc::new(AtomicBool::new(cli.play));
    let audio_song = std::sync::Arc::new(AtomicOptionBox::<Song>::none());
    let audio_seed = AudioSeed { time: audio_time.clone(), play: audio_play.clone(), song: audio_song.clone() };
    let audio = ami_boot_audio(audio_seed);

    enable_raw_mode()?;
    crossterm::execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut term = Terminal::new(backend)?;

    // This will be the file edit area.
    let mut textarea = if let Some(path) = cli.filename {
        let file = fs::File::open(path)?;
        io::BufReader::new(file)
            .lines()
            .collect::<io::Result<_>>()?
    } else {
        TextArea::default()
    };

    textarea.set_block(Mode::Normal.block());
    textarea.set_cursor_style(Mode::Normal.cursor_style());

    // This is the command-line-mode :entry box, which is only sometimes visible.
    let mut command = TextArea::default();
    command.set_block(Block::default().borders(Borders::NONE));
    command.set_cursor_style(Style::default().fg(Color::Reset).add_modifier(Modifier::REVERSED));

    let mut vim = Vim::new(Mode::Normal, VimAudioSeed { time:audio_time, play:audio_play.clone() }, false); // Note: time NOT cloned

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
                    let mut bottom_line_area = f.area();
                    bottom_line_area.y = bottom_line_area.height-1;
                    bottom_line_area.height=1;

                    if vim.mode.clone() == Mode::Command {
                        let bar = ratatui::widgets::Paragraph::new(":");
                        f.render_widget(bar, bottom_line_area);

                        bottom_line_area.x += 1;
                        bottom_line_area.width -= 1;
                        f.render_widget(&command, bottom_line_area);
                    } else {
                        let bar = ratatui::widgets::Paragraph::new(format!("{} {}", if (*vim.audio.play).load(Ordering::Relaxed) { "PLAYING" } else {"Paused "}, (*vim.audio.time).load(Ordering::Relaxed)));
                        f.render_widget(bar, bottom_line_area);
                    }
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
                        let transition; let mut dirty;
                        (transition, dirty) = vim.transition(event.into(), &mut textarea, &mut command);
                        dirty = dirty || vim.dirty;

                        match (transition.clone(), vim.mode) {
                            (Transition::Nop, Mode::Normal) |
                            (Transition::Mode(Mode::Normal), _) => {
                                if dirty {
                                    // Completely parse buffer
                                    // TODO: Factor elsewhere
                                    let mut line_starts:Vec<usize> = Default::default();
                                    let mut all:String = Default::default();
                                    // FIXME: Since I'm scanning twice, why not allocate an early buffer
                                    for line in textarea.lines() {
                                        line_starts.push(all.len());
                                        all = all + line;
                                    }
                                    let all = textarea.lines().join("\n");
                                    let song = if all.len() > 0 {parse_language(all)}
                                        else { Ok(Default::default()) }; // Empty string is valid
                                    //eprintln!("D: {:?}", song.clone());
                                    match song {
                                        Ok(song) => {
                                            // I *think* I don't need SeqCst because only one thread writes?
                                            let song = absolute_language(song);
                                            audio_song.store(Some(Box::new(song)), Ordering::AcqRel)
                                        },
                                        Err(error) => {
                                            // TODO: Reverse Bad position
                                            eprintln!("{}", error);
                                        }
                                    }

                                    dirty = false;
                                }
                            },
                            _ => ()
                        };

                        vim = match transition {
                            // Audio or UI changed
                            Transition::Mode(mode) if vim.mode != mode => {
                                textarea.set_block(mode.block());
                                textarea.set_cursor_style(mode.cursor_style());
                                Vim::new(mode, vim.audio, dirty)
                            }

                            // Only audio changed
                            Transition::Mode(mode) if vim.dirty != dirty => {
                                Vim::new(mode, vim.audio, dirty)
                            }
                            Transition::Nop if vim.dirty != dirty => {
                                Vim::new(vim.mode, vim.audio, dirty)
                            }

                            // Nothing changed
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
