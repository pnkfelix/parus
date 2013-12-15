#[pkgid="parus#0.9-pre"];
#[feature(managed_boxes)];
#[feature(macro_rules)];

extern mod extra;
extern mod syntax;
extern mod rustc;

use syntax::codemap;
use syntax::diagnostic;
use r_driver      = rustc::driver::driver;
use r_session     = rustc::driver::session;
use r_token       = syntax::parse::token;
use r_cstore      = rustc::metadata::cstore;
use r_filesearch  = rustc::metadata::filesearch;

use std::hashmap::{HashMap};
use std::os;

mod parse;

mod p_driver {
    use parse::{parse_crate_from_file, parse_crate_from_source_str};
    use Cfg      = syntax::ast::CrateConfig;
    use Crate    = syntax::ast::Crate;
    use Sess     = rustc::driver::session::Session;
    use r_driver = rustc::driver::driver;
    use Inp      = rustc::driver::driver::input;

    type OPath = Option<Path>;

    pub fn phase_1_parse_input(sess: Sess, cfg: Cfg, input: &Inp) -> Crate {
        use rustc::util::common::time;

        time(sess.time_passes(), "parsing", (), |_| {
                match *input {
                    r_driver::file_input(ref file) => {
                        parse_crate_from_file(
                            &(*file), cfg.clone(), sess.parse_sess)
                    }
                    r_driver::str_input(src) => {
                        let cfg2 = cfg.clone();
                        parse_crate_from_source_str(
                            r_driver::anon_src(), src, cfg2, sess.parse_sess)
                    }
                }
            })
    }

    pub fn compile_input(sess: Sess, cfg: Cfg, input: &Inp, outdir: &OPath, output: &OPath) {
        // We need nested scopes here, because the intermediate results can keep
        // large chunks of memory alive and we want to free them as soon as
        // possible to keep the peak memory usage low
        let (outputs, trans) = {
            let expanded_crate = {
                let crate = phase_1_parse_input(sess, cfg.clone(), input);
                debug!("phase 1 complete");
                if r_driver::stop_after_phase_1(sess) { return; }
                r_driver::phase_2_configure_and_expand(sess, cfg, crate)
            };
            debug!("phase 2 complete");
            let analysis =
                r_driver::phase_3_run_analysis_passes(sess, &expanded_crate);
            debug!("phase 3 complete");
            if r_driver::stop_after_phase_3(sess) { return; }
            let outputs = r_driver::build_output_filenames(
                input, outdir, output, expanded_crate.attrs, sess);
            let trans = r_driver::phase_4_translate_to_llvm(
                sess, expanded_crate, &analysis, outputs);
            debug!("phase 4 complete");
            (outputs, trans)
        };
        r_driver::phase_5_run_llvm_passes(sess, &trans, outputs);
        debug!("phase 5 complete");
        if r_driver::stop_after_phase_5(sess) { return; }
        r_driver::phase_6_link_output(sess, &trans, input, outputs);
        debug!("phase 6 complete");
    }
}

struct ParusEmitter;

type at_cm_s = (@codemap::CodeMap, codemap::Span);
type level = diagnostic::level;

impl diagnostic::Emitter for ParusEmitter {
    fn emit(&self, cmsp: Option<at_cm_s>, msg: &str, lvl: level) {
        diagnostic::DefaultEmitter.emit(cmsp, msg, lvl);
    }
}

pub fn build_session(sopts: @r_session::options, demitter: @diagnostic::Emitter)
                     -> r_session::Session {
    let codemap = @codemap::CodeMap::new();
    let diagnostic_handler =
        diagnostic::mk_handler(Some(demitter));
    let span_diagnostic_handler =
        diagnostic::mk_span_handler(diagnostic_handler, codemap);
    build_session_(sopts, codemap, demitter, span_diagnostic_handler)
}

pub fn build_session_(sopts: @r_session::options,
                      cm: @codemap::CodeMap,
                      demitter: @diagnostic::Emitter,
                      span_diagnostic_handler: @mut diagnostic::span_handler)
                      -> r_session::Session {
    let target_cfg = r_driver::build_target_config(sopts, demitter);
    let p_s = parse::new_parse_sess_special_handler(span_diagnostic_handler,
                                                    cm);
    let cstore = @mut r_cstore::mk_cstore(r_token::get_ident_interner());
    let filesearch = r_filesearch::mk_filesearch(
        &sopts.maybe_sysroot,
        sopts.target_triple,
        sopts.addl_lib_search_paths);
    @r_session::Session_ {
        targ_cfg: target_cfg,
        opts: sopts,
        cstore: cstore,
        parse_sess: p_s,
        codemap: cm,
        // For a library crate, this is always none
        entry_fn: @mut None,
        entry_type: @mut None,
        span_diagnostic: span_diagnostic_handler,
        filesearch: filesearch,
        building_library: @mut false,
        working_dir: os::getcwd(),
        lints: @mut HashMap::new(),
        node_id: @mut 1,
        outputs: @mut ~[],
    }
}

pub fn main() {
    use rustc::driver::session;
    use r_driver = rustc::driver::driver;

    debug!("parus outset");

    let diag_emitter = @ParusEmitter as @diagnostic::Emitter;
    // let matches = getopts::getopts(~[], ~[]).unwrap();
    // build_session_options(@"parus.out", matches, diag_emitter);
    let mut sopts = basic_options_();
    sopts.binary = @"parus.out";
    // sopts.debugging_opts |= rustc::driver::session::time_passes;
    let rust_rt_libdir = "/Users/pnkfelix/opt/rust-dbg/lib/rustc/x86_64-apple-darwin/lib";
    sopts.addl_lib_search_paths.insert(Path::new(rust_rt_libdir));
    sopts.linker_args.push(~"-L" + rust_rt_libdir);
    let sopts = @sopts.clone();

    debug!("parus build_session");
    let sess = build_session(sopts, diag_emitter);

    debug!("parus build_configuration");
    let cfg = r_driver::build_configuration(sess);
    let input = r_driver::file_input(Path::new("paren.input"));
    let outdir  : Option<Path> = None;
    let outfile : Option<Path> = Some(Path::new("paren.out"));

    debug!("parus compile_input start");
    p_driver::compile_input(sess, cfg, &input, &outdir, &outfile);
    debug!("parus compile_input finis");

    /// Some reasonable defaults
    fn basic_options_() -> session::options {
        use rustc::back::link;
        use rustc::driver::driver::host_triple;
        use std::hashmap::HashSet;

        session::options {
            outputs: ~[],
            gc: false,
            optimize: session::No,
            custom_passes: ~[],
            llvm_args: ~[],
            debuginfo: false,
            extra_debuginfo: false,
            lint_opts: ~[],
            save_temps: false,
            output_type: link::output_type_exe,
            addl_lib_search_paths: @mut HashSet::new(),
            ar: None,
            linker: None,
            linker_args: ~[],
            maybe_sysroot: None,
            target_triple: host_triple(),
            target_cpu: ~"generic",
            target_feature: ~"",
            cfg: ~[],
            binary: @"rustc",
            test: false,
            parse_only: false,
            no_trans: false,
            debugging_opts: 0u,
            android_cross_path: None,
            write_dependency_info: false,
        }
    }
}
