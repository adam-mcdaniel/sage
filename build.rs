extern crate lalrpop;

fn main() {
    let _ = lalrpop::Configuration::new()
        .emit_rerun_directives(true)
        .process_current_dir();
}
