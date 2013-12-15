#[allow(dead_code)]

type c_char = i8;

#[lang="start"]
pub fn start(main: *u8, _argc: int, _argv: **c_char) -> int {
    unsafe {
        let main: extern "Rust" fn() = intrinsics::transmute(main);
        main();
    }
    return 0;
}

pub mod intrinsics {
    extern "rust-intrinsic" {
        /// Abort the execution of the process.
        pub fn abort() -> !;
        /// Execute a breakpoint trap, for inspection by a debugger.
        pub fn breakpoint();
        pub fn transmute<T,U>(e: T) -> U;
    }
}

#[no_split_stack] // - it would be sad for this function to trigger __morestack
pub extern "C" fn rust_stack_exhausted() {
    unsafe {
        intrinsics::abort();
    }
}
