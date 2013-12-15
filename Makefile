default: run-parus

run-parus: parus paren.input
	RUST_LOG=parus,rustc::driver ./parus

parus: lib.rs *.rs parse/*.rs
	rustc lib.rs
