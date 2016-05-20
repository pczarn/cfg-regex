extern crate cfg;
extern crate regex_dfa;
extern crate regex_syntax;

pub use regex_syntax::ClassRange;

use std::collections::BTreeMap;

use cfg::{Cfg, Symbol, ContextFree};
use cfg::history::RewriteSequence;
use regex_syntax::{Expr, ExprBuilder, Result, CharClass, Repeater};

pub struct RegexTranslation {
    // Here, Vec<ClassRange> is the content of CharClass.
    classes: BTreeMap<Vec<ClassRange>, Symbol>,
    // Every distinct range has a terminal symbol.
    ranges: BTreeMap<ClassRange, Symbol>,
    // A nulling symbol, if used.
    empty: Option<Symbol>,
}

impl RegexTranslation {
    pub fn new() -> Self {
        RegexTranslation {
            classes: BTreeMap::new(),
            ranges: BTreeMap::new(),
            empty: None,
        }
    }

    pub fn rewrite_string<H>(&mut self, cfg: &mut Cfg<H>, string: &str) -> Symbol
        where H: Clone + Default + RewriteSequence<Rewritten = H>
    {
        let factors = string.chars().map(|ch| self.rewrite_char(cfg, ch)).collect::<Vec<_>>();
        let lhs = cfg.sym();
        cfg.rule(lhs).rhs_with_history(&factors[..], H::default());
        lhs
    }

    pub fn rewrite_regex<H>(&mut self, cfg: &mut Cfg<H>, regex: &str) -> Result<Symbol>
        where H: Clone + Default + RewriteSequence<Rewritten = H>
    {
        ExprBuilder::new().unicode(true).allow_bytes(false).parse(regex).map(|expr| {
            self.rewrite_expr(cfg, &expr)
        })
    }

    fn rewrite_expr<H>(&mut self, cfg: &mut Cfg<H>, expr: &Expr) -> Symbol
        where H: Clone + Default + RewriteSequence<Rewritten = H>
    {
        match *expr {
            Expr::Empty => {
                self.empty(cfg)
            }
            Expr::Literal { ref chars, casei } => {
                assert!(!casei);
                let factors = chars.iter().map(|&ch| {
                    self.rewrite_char(cfg, ch)
                }).collect::<Vec<_>>();
                let lhs = cfg.sym();
                cfg.rule(lhs).rhs_with_history(&factors[..], H::default());
                lhs
            }
            Expr::Class(ref class) => {
                self.rewrite_class(cfg, class)
            }
            Expr::Concat(ref factors) => {
                let mut rhs = vec![];
                for factor in factors {
                    let factor_sym = self.rewrite_expr(cfg, factor);
                    rhs.push(factor_sym);
                }
                let lhs = cfg.sym();
                cfg.rule(lhs).rhs_with_history(&rhs[..], H::default());
                lhs
            }
            Expr::Alternate(ref sum) => {
                let lhs = cfg.sym();
                for summand in sum {
                    let summand_sym = self.rewrite_expr(cfg, summand);
                    cfg.rule(lhs).rhs_with_history([summand_sym], H::default());
                }
                lhs
            }
            Expr::Repeat { e: ref inner_expr, r: repetition, greedy } => {
                assert!(greedy);
                let (min, max) = match repetition {
                    Repeater::Range { min, max } => (min, max),
                    Repeater::ZeroOrOne => (0, Some(1)),
                    Repeater::ZeroOrMore => (0, None),
                    Repeater::OneOrMore => (1, None),
                };
                let lhs = cfg.sym();
                let inner_sym = self.rewrite_expr(cfg, &**inner_expr);
                cfg.sequence(lhs).inclusive(min, max).rhs_with_history(inner_sym, H::default());
                lhs
            }
            _ => panic!()
        }
    }

    fn rewrite_char<H>(&mut self, cfg: &mut Cfg<H>, ch: char) -> Symbol
        where H: Clone + Default + RewriteSequence<Rewritten = H>
    {
        let range = ClassRange { start: ch, end: ch };
        *self.ranges.entry(range).or_insert_with(|| {
            cfg.sym()
        })
    }

    fn rewrite_class<H>(&mut self, cfg: &mut Cfg<H>, class: &CharClass) -> Symbol
        where H: Clone + Default + RewriteSequence<Rewritten = H>
    {
        let ranges = &mut self.ranges;
        *self.classes.entry((**class).clone()).or_insert_with(|| {
            let class_sym = cfg.sym();
            for &range in class {
                let range_sym = *ranges.entry(range).or_insert_with(|| {
                    cfg.sym()
                });
                cfg.rule(class_sym).rhs_with_history([range_sym], H::default());
            }
            class_sym
        })
    }

    fn empty<H>(&mut self, cfg: &mut Cfg<H>) -> Symbol
        where H: Clone + Default + RewriteSequence<Rewritten = H>
    {
        if let Some(empty) = self.empty {
            empty
        } else {
            let empty = cfg.sym();
            cfg.rule(empty).rhs_with_history([], H::default());
            self.empty = Some(empty);
            empty
        }
    }

    pub fn get_ranges(&self) -> &BTreeMap<ClassRange, Symbol> {
        &self.ranges
    }
}
