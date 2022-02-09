use cranelisp::{env::get_env, lexer::Lexer, parser::Parser, translate::Translator, vm::VM};
use cursive::{
    event::Event,
    traits::{Nameable, Resizable},
    views::{DummyView, LinearLayout, ListView, Menubar, TextArea, TextView},
    Cursive, CursiveRunnable,
};
use once_cell::unsync::Lazy;
use somok::{Leaksome, Somok};

fn main() {
    let mut siv = init_ui();
    siv.run();
}

static mut VM: Lazy<&'static mut VM> =
    Lazy::new(|| VM::new(vec![], 100, cranelisp::env::setup()).boxed().leak());

fn init_ui() -> CursiveRunnable {
    let mut siv = cursive::default();

    let env_panel = LinearLayout::vertical()
        .child(TextView::new("Environment"))
        .child(ListView::new().with_name("env"));
    let stack_panel = LinearLayout::vertical()
        .child(TextView::new("Stack"))
        .child(ListView::new().with_name("stack"));

    let history_panel = LinearLayout::vertical()
        .child(TextView::new("History"))
        .child(ListView::new().with_name("history"))
        .full_width();
    let panels = LinearLayout::horizontal()
        .child(env_panel)
        .child(DummyView)
        .child(history_panel)
        .child(DummyView)
        .child(stack_panel)
        .child(DummyView)
        .full_width()
        .full_height();
    let mut menu = Menubar::new();

    siv.add_global_callback(Event::CtrlChar('q'), Cursive::quit);
    menu.add_leaf("[C+q] Quit", Cursive::quit);

    let run_cb = |s: &mut Cursive| {
        let mut line = "".to_string();

        s.call_on_name("input", |i: &mut TextArea| {
            line = i.get_content().to_string();
            i.set_content("");
        });

        let mut lexer = Lexer::new(line.clone()).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let tt = parser.parse_expr().unwrap();
        let mut trans = Translator::new();
        let code = trans.translate(tt.0);
        let code = trans.compile(code);
        let (res, env) = unsafe {
            VM.replace_code(code);
            VM.reset_ip();

            let res = format!("> {:?}", VM.run());
            let env = VM.env();
            (res, env)
        };

        s.call_on_name("env", |e: &mut ListView| {
            e.clear();
            for (k, v) in get_env(env).iter() {
                e.add_child(&*k, TextView::new(format!("{:?}", &**v)))
            }
        });

        s.call_on_name("history", |h: &mut ListView| {
            h.add_child(&line, DummyView);
            h.add_child(&res, DummyView);
        });
    };
    siv.add_global_callback(Event::CtrlChar('r'), run_cb);
    menu.add_leaf("[C+r] Run", run_cb);

    let linear_layout = LinearLayout::vertical()
        .child(panels)
        .child(TextArea::new().with_name("input"))
        .child(menu.full_width())
        .full_screen();

    siv.add_layer(linear_layout);
    siv
}
