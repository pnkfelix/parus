use syntax::ast;
use syntax::codemap;
use syntax::diagnostic;
use syntax::parse::lexer;
use syntax::parse::lexer::reader;
use syntax::parse::ParseSess;
use syntax::parse::obsolete::ParserObsoleteMethods;

use std::io;
use std::io::File;
use std::str;
use std::vec;
use self::attr::parser_attr;

mod parser;

mod attr {
    use syntax::ast;
    use syntax::codemap;
    use r_attr = syntax::attr;
    use r_parse = syntax::parse;
    use r_token = syntax::parse::token;

    // a parser that can parse attributes.
    pub trait parser_attr {
        fn parse_outer_attributes(&self) -> ~[ast::Attribute];
        fn parse_attribute(&self, permit_inner: bool) -> ast::Attribute;
        fn parse_inner_attrs_and_next(&self) ->
            (~[ast::Attribute], ~[ast::Attribute]);
        fn parse_meta_item(&self) -> @ast::MetaItem;
        fn parse_meta_seq(&self) -> ~[@ast::MetaItem];
        fn parse_optional_meta(&self) -> ~[@ast::MetaItem];
    }

    impl parser_attr for ::parse::parser::Parser {
        // Parse attributes that appear before an item
        fn parse_outer_attributes(&self) -> ~[ast::Attribute] {
            let mut attrs: ~[ast::Attribute] = ~[];
            loop {
                debug!("parse_outer_attributes: self.token={:?}",
                       self.token);
                match *self.token {
                    r_token::INTERPOLATED(r_token::nt_attr(..)) => {
                        attrs.push(self.parse_attribute(false));
                    }
                    r_token::POUND => {
                        if self.look_ahead(1, |t| *t != r_token::LBRACKET) {
                            break;
                        }
                        attrs.push(self.parse_attribute(false));
                    }
                    r_token::DOC_COMMENT(s) => {
                        let attr = r_attr::mk_sugared_doc_attr(
                            self.id_to_str(s),
                            self.span.lo,
                            self.span.hi
                                );
                        if attr.node.style != ast::AttrOuter {
                        self.fatal("expected outer comment");
                        }
                        attrs.push(attr);
                        self.bump();
                    }
                    _ => break
                }
            }
            return attrs;
        }

        // matches attribute = # [ meta_item ]
        //
        // if permit_inner is true, then a trailing `;` indicates an inner
        // attribute
        fn parse_attribute(&self, permit_inner: bool) -> ast::Attribute {
            debug!("parse_attributes: permit_inner={:?} self.token={:?}",
                   permit_inner, self.token);
            let (span, value) = match *self.token {
                r_token::INTERPOLATED(r_token::nt_attr(attr)) => {
                    assert!(attr.node.style == ast::AttrOuter);
                    self.bump();
                    (attr.span, attr.node.value)
                }
                r_token::POUND => {
                    let lo = self.span.lo;
                    self.bump();
                    self.expect(&r_token::LBRACKET);
                    let meta_item = self.parse_meta_item();
                    self.expect(&r_token::RBRACKET);
                    let hi = self.span.hi;
                    (codemap::mk_sp(lo, hi), meta_item)
                }
                _ => {
                    self.fatal(format!("expected `\\#` but found `{}`",
                                       self.this_token_to_str()));
                }
            };
            let style = if permit_inner && *self.token == r_token::SEMI {
                self.bump();
                ast::AttrInner
            } else {
                ast::AttrOuter
            };
            return codemap::Spanned {
                span: span,
                node: ast::Attribute_ {
                    style: style,
                    value: value,
                    is_sugared_doc: false
                }
            };
        }

        // Parse attributes that appear after the opening of an item, each
        // terminated by a semicolon. In addition to a vector of inner attributes,
        // this function also returns a vector that may contain the first outer
        // attribute of the next item (since we can't know whether the attribute
        // is an inner attribute of the containing item or an outer attribute of
        // the first contained item until we see the semi).

        // matches inner_attrs* outer_attr?
        // you can make the 'next' field an Option, but the result is going to be
        // more useful as a vector.
        fn parse_inner_attrs_and_next(&self)
                                      -> (~[ast::Attribute], ~[ast::Attribute]) {
            let mut inner_attrs: ~[ast::Attribute] = ~[];
            let mut next_outer_attrs: ~[ast::Attribute] = ~[];
            loop {
                let attr = match *self.token {
                    r_token::INTERPOLATED(r_token::nt_attr(..)) => {
                        self.parse_attribute(true)
                    }
                    r_token::POUND => {
                        if self.look_ahead(1, |t| *t != r_token::LBRACKET) {
                            // This is an extension
                            break;
                        }
                        self.parse_attribute(true)
                    }
                    r_token::DOC_COMMENT(s) => {
                        self.bump();
                        r_attr::mk_sugared_doc_attr(self.id_to_str(s),
                                                    self.span.lo,
                                                    self.span.hi)
                    }
                    _ => {
                        break;
                    }
                };
                if attr.node.style == ast::AttrInner {
                    inner_attrs.push(attr);
                } else {
                    next_outer_attrs.push(attr);
                    break;
                }
            }
            (inner_attrs, next_outer_attrs)
        }

        // matches meta_item = IDENT
        // | IDENT = lit
        // | IDENT meta_seq
        fn parse_meta_item(&self) -> @ast::MetaItem {
            let lo = self.span.lo;
            let name = self.id_to_str(self.parse_ident());
            match *self.token {
                r_token::EQ => {
                    self.bump();
                    let lit = self.parse_lit();
                    // FIXME #623 Non-string meta items are not serialized correctly;
                    // just forbid them for now
                    match lit.node {
                        ast::lit_str(..) => (),
                        _ => {
                            self.span_err(
                                lit.span,
                                "non-string literals are not allowed in meta-items");
                        }
                    }
                    let hi = self.span.hi;
                    @codemap::spanned(lo, hi, ast::MetaNameValue(name, lit))
                }
                r_token::LPAREN => {
                    let inner_items = self.parse_meta_seq();
                    let hi = self.span.hi;
                    @codemap::spanned(lo, hi, ast::MetaList(name, inner_items))
                }
                _ => {
                    let hi = self.last_span.hi;
                    @codemap::spanned(lo, hi, ast::MetaWord(name))
                }
            }
        }

        // matches meta_seq = ( COMMASEP(meta_item) )
        fn parse_meta_seq(&self) -> ~[@ast::MetaItem] {
            self.parse_seq(&r_token::LPAREN,
                           &r_token::RPAREN,
                           r_parse::common::seq_sep_trailing_disallowed(r_token::COMMA),
                           |p| p.parse_meta_item()).node
        }

        fn parse_optional_meta(&self) -> ~[@ast::MetaItem] {
            match *self.token {
                r_token::LPAREN => self.parse_meta_seq(),
                _ => ~[]
            }
        }
    }
}

fn maybe_append(lhs: ~[ast::Attribute], rhs: Option<~[ast::Attribute]>)
                -> ~[ast::Attribute] {
    match rhs {
        None => lhs,
        Some(ref attrs) => vec::append(lhs, (*attrs))
    }
}

type Parser = parser::Parser;

pub fn new_parse_sess(demitter: Option<@diagnostic::Emitter>) -> @mut ParseSess {
    let cm = @codemap::CodeMap::new();
    @mut ParseSess {
        cm: cm,
        span_diagnostic: diagnostic::mk_span_handler(diagnostic::mk_handler(demitter), cm),
        included_mod_stack: ~[],
    }
}

pub fn new_parse_sess_special_handler(sh: @mut diagnostic::span_handler,
                                      cm: @codemap::CodeMap)
                                   -> @mut ParseSess {
    @mut ParseSess {
        cm: cm,
        span_diagnostic: sh,
        included_mod_stack: ~[],
    }
}

// a bunch of utility functions of the form parse_<thing>_from_<source>
// where <thing> includes crate, expr, item, stmt, tts, and one that
// uses a HOF to parse anything, and <source> includes file and
// source_str.

pub fn parse_crate_from_file(
    input: &Path,
    cfg: ast::CrateConfig,
    sess: @mut ParseSess
) -> ast::Crate {
    new_parser_from_file(sess, /*bad*/ cfg.clone(), input).parse_crate_mod()
    // why is there no p.abort_if_errors here?
}

pub fn parse_crate_from_source_str(
    name: @str,
    source: @str,
    cfg: ast::CrateConfig,
    sess: @mut ParseSess
) -> ast::Crate {
    let p = new_parser_from_source_str(sess,
                                       /*bad*/ cfg.clone(),
                                       name,
                                       source);
    maybe_aborted(p.parse_crate_mod(),p)
}

pub fn parse_expr_from_source_str(
    name: @str,
    source: @str,
    cfg: ast::CrateConfig,
    sess: @mut ParseSess
) -> @ast::Expr {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        name,
        source
    );
    maybe_aborted(p.parse_expr(), p)
}

pub fn parse_item_from_source_str(
    name: @str,
    source: @str,
    cfg: ast::CrateConfig,
    attrs: ~[ast::Attribute],
    sess: @mut ParseSess
) -> Option<@ast::item> {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        name,
        source
    );
    maybe_aborted(p.parse_item(attrs),p)
}

pub fn parse_meta_from_source_str(
    name: @str,
    source: @str,
    cfg: ast::CrateConfig,
    sess: @mut ParseSess
) -> @ast::MetaItem {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        name,
        source
    );
    maybe_aborted(p.parse_meta_item(),p)
}

pub fn parse_stmt_from_source_str(
    name: @str,
    source: @str,
    cfg: ast::CrateConfig,
    attrs: ~[ast::Attribute],
    sess: @mut ParseSess
) -> @ast::Stmt {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        name,
        source
    );
    maybe_aborted(p.parse_stmt(attrs),p)
}

pub fn parse_tts_from_source_str(
    name: @str,
    source: @str,
    cfg: ast::CrateConfig,
    sess: @mut ParseSess
) -> ~[ast::token_tree] {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        name,
        source
    );
    *p.quote_depth += 1u;
    // right now this is re-creating the token trees from ... token trees.
    maybe_aborted(p.parse_all_token_trees(),p)
}

// given a function and parsing information (source str,
// filename, crate cfg, and sess), create a parser,
// apply the function, and check that the parser
// consumed all of the input before returning the function's
// result.
pub fn parse_from_source_str<T>(
                             f: |&Parser| -> T,
                             name: @str,
                             ss: codemap::FileSubstr,
                             source: @str,
                             cfg: ast::CrateConfig,
                             sess: @mut ParseSess)
                             -> T {
    let p = new_parser_from_source_substr(sess, cfg, name, ss, source);
    let r = f(&p);
    if !p.reader.is_eof() {
        p.reader.fatal(~"expected end-of-string");
    }
    maybe_aborted(r,p)
}

// Create a new parser from a source string
pub fn new_parser_from_source_str(sess: @mut ParseSess,
                                  cfg: ast::CrateConfig,
                                  name: @str,
                                  source: @str)
                               -> Parser {
    filemap_to_parser(sess,string_to_filemap(sess,source,name),cfg)
}

// Create a new parser from a source string where the origin
// is specified as a substring of another file.
pub fn new_parser_from_source_substr(sess: @mut ParseSess,
                                  cfg: ast::CrateConfig,
                                  name: @str,
                                  ss: codemap::FileSubstr,
                                  source: @str)
                               -> Parser {
    filemap_to_parser(sess,substring_to_filemap(sess,source,name,ss),cfg)
}

/// Create a new parser, handling errors as appropriate
/// if the file doesn't exist
pub fn new_parser_from_file(
    sess: @mut ParseSess,
    cfg: ast::CrateConfig,
    path: &Path
) -> Parser {
    filemap_to_parser(sess,file_to_filemap(sess,path,None),cfg)
}

/// Given a session, a crate config, a path, and a span, add
/// the file at the given path to the codemap, and return a parser.
/// On an error, use the given span as the source of the problem.
pub fn new_sub_parser_from_file(
    sess: @mut ParseSess,
    cfg: ast::CrateConfig,
    path: &Path,
    sp: codemap::Span
) -> Parser {
    filemap_to_parser(sess,file_to_filemap(sess,path,Some(sp)),cfg)
}

/// Given a filemap and config, return a parser
pub fn filemap_to_parser(sess: @mut ParseSess,
                         filemap: @codemap::FileMap,
                         cfg: ast::CrateConfig) -> Parser {
    tts_to_parser(sess,filemap_to_tts(sess,filemap),cfg)
}

// must preserve old name for now, because quote! from the *existing*
// compiler expands into it
pub fn new_parser_from_tts(sess: @mut ParseSess,
                     cfg: ast::CrateConfig,
                     tts: ~[ast::token_tree]) -> Parser {
    tts_to_parser(sess,tts,cfg)
}


// base abstractions

/// Given a session and a path and an optional span (for error reporting),
/// add the path to the session's codemap and return the new filemap.
pub fn file_to_filemap(sess: @mut ParseSess, path: &Path, spanopt: Option<codemap::Span>)
    -> @codemap::FileMap {
    let err = |msg: &str| {
        match spanopt {
            Some(sp) => sess.span_diagnostic.span_fatal(sp, msg),
            None => sess.span_diagnostic.handler().fatal(msg),
        }
    };
    let bytes = match io::result(|| File::open(path).read_to_end()) {
        Ok(bytes) => bytes,
        Err(e) => {
            err(format!("couldn't read {}: {}", path.display(), e.desc));
            unreachable!()
        }
    };
    match str::from_utf8_owned_opt(bytes) {
        Some(s) => {
            return string_to_filemap(sess, s.to_managed(),
                                     path.as_str().unwrap().to_managed());
        }
        None => {
            err(format!("{} is not UTF-8 encoded", path.display()))
        }
    }
    unreachable!()
}

// given a session and a string, add the string to
// the session's codemap and return the new filemap
pub fn string_to_filemap(sess: @mut ParseSess, source: @str, path: @str)
    -> @codemap::FileMap {
    sess.cm.new_filemap(path, source)
}

// given a session and a string and a path and a FileSubStr, add
// the string to the CodeMap and return the new FileMap
pub fn substring_to_filemap(sess: @mut ParseSess, source: @str, path: @str,
                           filesubstr: codemap::FileSubstr) -> @codemap::FileMap {
    sess.cm.new_filemap_w_substr(path,filesubstr,source)
}

// given a filemap, produce a sequence of token-trees
pub fn filemap_to_tts(sess: @mut ParseSess, filemap: @codemap::FileMap)
    -> ~[ast::token_tree] {
    // it appears to me that the cfg doesn't matter here... indeed,
    // parsing tt's probably shouldn't require a parser at all.
    let cfg = ~[];
    let srdr = lexer::new_string_reader(sess.span_diagnostic, filemap);
    let p1 = parser::Parser(sess, cfg, srdr as @mut reader);
    p1.parse_all_token_trees()
}

// given tts and cfg, produce a parser
pub fn tts_to_parser(sess: @mut ParseSess,
                     tts: ~[ast::token_tree],
                     cfg: ast::CrateConfig) -> Parser {
    let trdr = lexer::new_tt_reader(sess.span_diagnostic, None, tts);
    parser::Parser(sess, cfg, trdr as @mut reader)
}

// abort if necessary
pub fn maybe_aborted<T>(result : T, p: Parser) -> T {
    p.abort_if_errors();
    result
}
