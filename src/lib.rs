extern crate cfg;
extern crate regex_syntax;

pub use regex_syntax::hir::{
    Hir,
    HirKind,
    Literal,
    Class,
    ClassUnicode,
    ClassBytes,
    ClassUnicodeRange,
    ClassBytesRange,
    RepetitionKind,
    RepetitionRange,
};

use std::iter;

use cfg::{Cfg, Symbol, ContextFree};
use cfg::history::RewriteSequence;
use regex_syntax::{Result, Parser};

struct ClassMap {
    map: Vec<(Class, Symbol)>,
}

impl ClassMap {
    fn get(&self, key: &Class) -> Option<Symbol> {
        for &(ref class, sym) in &self.map {
            if class == key {
                return Some(sym);
            }
        }
        None
    }

    fn insert(&mut self, key: Class, value: Symbol) -> Option<Symbol> {
        for &mut (ref class, ref mut sym) in &mut self.map {
            if class == &key {
                let old_value = *sym;
                *sym = value;
                return Some(old_value);
            }
        }
        self.map.push((key, value));
        None
    }

    fn contains_class(&self, key: &Class) -> bool {
        for &(ref class, _) in &self.map {
            if class == key {
                return true;
            }
        }
        false
    }

    fn get_or_insert_with<F>(&mut self, key: Class, f: F) -> Symbol
        where F: FnOnce() -> Symbol,
    {
        for &(ref class, sym) in &self.map {
            if class == &key {
                return sym;
            }
        }
        let sym = f();
        self.map.push((key, sym));
        sym
    }
}

pub struct RegexTranslation<'a, H> {
    // Every distinct class has a terminal or nonterminal symbol.
    // Empty class [] points to a nulling symbol, if used.
    classes: ClassMap,
    // A grammar.
    cfg: &'a mut Cfg<H>,
}

impl<'a, H> RegexTranslation<'a, H>
    where H: Clone + Default + RewriteSequence<Rewritten = H>
{
    pub fn new(cfg: &'a mut Cfg<H>) -> RegexTranslation<'a, H> {
        RegexTranslation {
            classes: ClassMap {
                map: vec![]
            },
            cfg,
        }
    }

    pub fn change_cfg<'b, H2>(self, other_cfg: &'b mut Cfg<H2>) -> RegexTranslation<'b, H2>
        where H2: Clone + Default + RewriteSequence<Rewritten = H>
    {
        RegexTranslation {
            classes: self.classes,
            cfg: other_cfg,
        }
    }
    
    pub fn rewrite_string(&mut self, string: &str) -> Symbol {
        let factors = string.chars().map(|ch| self.rewrite_char(ch)).collect::<Vec<_>>();
        let lhs = self.cfg.sym();
        self.cfg.rule(lhs).rhs_with_history(&factors[..], H::default());
        lhs
    }

    pub fn rewrite_regex(&mut self, regex: &str) -> Result<Symbol> {
        Parser::new().parse(regex).map(|hir| self.rewrite_hir(&hir))
    }

    fn rewrite_hir(&mut self, hir: &Hir) -> Symbol {
        match hir.kind() {
            &HirKind::Empty => {
                self.empty()
            }
            &HirKind::Literal(Literal::Unicode(ch)) => {
                self.rewrite_char(ch)
            }
            &HirKind::Literal(Literal::Byte(byte)) => {
                self.rewrite_byte(byte)
            }
            &HirKind::Class(ref class) => {
                self.rewrite_class(class)
            }
            &HirKind::Concat(ref factors) => {
                let mut rhs = vec![];
                for factor in factors {
                    let factor_sym = self.rewrite_hir(factor);
                    rhs.push(factor_sym);
                }
                let lhs = self.cfg.sym();
                self.cfg.rule(lhs).rhs_with_history(&rhs[..], H::default());
                lhs
            }
            // Beware! This code was written after some strong alcohocil drinks.
            &HirKind::Repetition(ref repetition) => {
                assert!(repetition.greedy);
                let (min, max) = match repetition.kind {
                    RepetitionKind::Range(RepetitionRange::Exactly(x)) => (x, Some(x)),
                    RepetitionKind::Range(RepetitionRange::AtLeast(x)) => (x, None),
                    RepetitionKind::Range(RepetitionRange::Bounded(x, y)) => (x, Some(y)),
                    RepetitionKind::ZeroOrOne => (0, Some(1)),
                    RepetitionKind::ZeroOrMore => (0, None),
                    RepetitionKind::OneOrMore => (1, None),
                };
                let lhs = self.cfg.sym();
                let inner_sym = self.rewrite_hir(&*repetition.hir);
                self.cfg.sequence(lhs).inclusive(min, max).rhs_with_history(inner_sym, H::default());
                lhs
            }
            &HirKind::Group(ref group) => {
                let lhs = self.cfg.sym();
                let inner_sym = self.rewrite_hir(&*group.hir);
                self.cfg.rule(lhs).rhs_with_history([inner_sym], H::default());
                lhs
            }
            &HirKind::Alternation(ref summands) => {
                let lhs = self.cfg.sym();
                for summand in summands {
                    let summand_sym = self.rewrite_hir(summand);
                    self.cfg.rule(lhs).rhs_with_history([summand_sym], H::default());
                }
                lhs
            }
            &HirKind::Anchor(..) | &HirKind::WordBoundary(..) => {
                panic!();
            }
        }
    }

    fn rewrite_char(&mut self, ch: char) -> Symbol {
        let range = ClassUnicodeRange::new(ch, ch);
        let class = ClassUnicode::new(iter::once(range));
        let cfg = &mut self.cfg;
        self.classes.get_or_insert_with(Class::Unicode(class), || {
            cfg.sym()
        })
    }

    fn rewrite_byte(&mut self, byte: u8) -> Symbol {
        let range = ClassBytesRange::new(byte, byte);
        let class = ClassBytes::new(iter::once(range));
        let cfg = &mut self.cfg;
        self.classes.get_or_insert_with(Class::Bytes(class), || {
            cfg.sym()
        })
    }

    fn rewrite_class(&mut self, class: &Class) -> Symbol {
        match class {
            &Class::Unicode(ref unicode) => {
                if self.classes.contains_class(class) {
                    self.classes.get(class).unwrap()
                } else {
                    let class_sym = self.cfg.sym();
                    for range in unicode.iter() {
                        let key = Class::Unicode(ClassUnicode::new(iter::once(range.clone())));
                        let cfg = &mut self.cfg;
                        let range_sym = self.classes.get_or_insert_with(key, || {
                            cfg.sym()
                        });
                        // if unicode.ranges().len() > 1 {
                        // else simplify
                        // }
                        self.cfg.rule(class_sym).rhs_with_history([range_sym], H::default());
                    }
                    self.classes.insert(class.clone(), class_sym);
                    class_sym
                }
            }
            &Class::Bytes(ref bytes) => {
                if self.classes.contains_class(class) {
                    self.classes.get(class).unwrap()
                } else {
                    let class_sym = self.cfg.sym();
                    for range in bytes.iter() {
                        let key = Class::Bytes(ClassBytes::new(iter::once(range.clone())));
                        let cfg = &mut self.cfg;
                        let range_sym = self.classes.get_or_insert_with(key, || {
                            cfg.sym()
                        });
                        // if unicode.ranges().len() > 1 {
                        // else simplify
                        // }
                        self.cfg.rule(class_sym).rhs_with_history([range_sym], H::default());
                    }
                    self.classes.insert(class.clone(), class_sym);
                    class_sym
                }
            }
        }
    }

    fn empty(&mut self) -> Symbol {
        let empty_class_unicode = Class::Unicode(ClassUnicode::empty());
        let empty_class_bytes = Class::Bytes(ClassBytes::empty());
        let cfg = &mut self.cfg;
        let value = self.classes.get_or_insert_with(empty_class_unicode, || {
            let empty = cfg.sym();
            cfg.rule(empty).rhs_with_history([], H::default());
            empty
        });
        self.classes.insert(empty_class_bytes, value);
        value
    }

    pub fn class_map(&self) -> &Vec<(Class, Symbol)> {
        &self.classes.map
    }
}
