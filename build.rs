extern crate lalrpop;

fn main() {
    let _ = lalrpop::Configuration::new()
        .emit_rerun_directives(false)
        .process_current_dir();
}
