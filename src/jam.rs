use slog::{info, o};

use crate::{
    config::{DesugaredConfig, Options},
    executor::Executor,
    store::{
        ExecError, ExecResult, Lookup, NextKey, ParseResult, Shortcut, Target, TargetStore,
        TrieDagStore,
    },
};

pub struct Jam<'a> {
    opts: &'a Options,
    executor: Executor,
    // TODO: Should this be a dyn trait?
    target_store: TrieDagStore<'a>,
    logger: slog::Logger,
}

impl<'a> Jam<'a> {
    pub fn new(
        logger: &slog::Logger,
        executor: Executor,
        cfg: &'a DesugaredConfig,
    ) -> ParseResult<Jam<'a>> {
        Ok(Jam {
            // NOTE: If we ever end up having a distinction between
            // config-time options vs. run-time options, we may want
            // to create a separate type here for isolation.
            opts: &cfg.options,
            executor,
            target_store: TrieDagStore::new(logger, &cfg.targets)?,
            logger: logger.new(o!()),
        })
    }

    /// Returns a mapping of all available shortcuts to their associated target
    /// names. The returned shortcuts are already reconciled and unambiguous.
    pub fn mappings(&self) -> ExecResult<Vec<(Shortcut, &str)>> {
        self.target_store
            .mappings(self.opts.reconciliation_strategy)
    }

    /// Takes a shortcut and determines what all the possible subsequent keys
    /// and their associated targets could be. In other words, it advances the
    /// current shortcut by computing all the possibilities that come next.
    pub fn next(
        &self,
        prefix: &Shortcut,
        conflict: bool, // NOTE: Given just a prefix, there isn't any way to know for sure if we are facing a conflict. Therefore, this information must be passed in from the context of the callsite.
    ) -> ExecResult<Vec<NextKey>> {
        self.target_store.next(
            prefix,
            if conflict {
                Some(self.opts.reconciliation_strategy)
            } else {
                None
            },
        )
    }

    /// Takes a shortcut and returns an existence result.
    /// The result includes the case where the shortcut leads to an ambiguity
    /// that must be reconciled.
    pub fn lookup(&self, shortcut: &Shortcut) -> Lookup {
        self.target_store
            .lookup(self.opts.reconciliation_strategy, shortcut)
    }

    pub fn execute_by_shortcut(&self, shortcut: Shortcut) -> ExecResult<()> {
        info!(self.logger, "executing by shortcut");
        let target = self
            .target_store
            .get_by_shortcut(self.opts.reconciliation_strategy, shortcut)?;
        self.execute_target(&self.logger, target, 0)
    }

    pub fn execute_by_target_name(&self, target_name: &str) -> ExecResult<()> {
        info!(self.logger, "executing by target name");
        let target = self.target_store.get_by_target_name(target_name)?;
        self.execute_target(&self.logger, target, 0)
    }

    // NOTE: We take a logger explicitly here instead of relying on self.logger
    // because this function can execute recursively, and we want to encode that
    // recursion into the logger's context as it goes on.
    fn execute_target(
        &self,
        logger: &slog::Logger,
        target: &Target,
        depth: usize,
    ) -> ExecResult<()> {
        let deps = self.target_store.children(target)?;
        for dep in deps {
            let dep_logger = logger.new(o!("parent" => target.name.to_string()));
            self.execute_target(&dep_logger, dep, depth + 1)
                .map_err(|err| ExecError::Dependency {
                    dep_name: target.name.to_string(),
                    err: Box::new(err),
                })?;
        }
        if let Some(cmd) = target.cmd {
            info!(logger, "running executor"; o!("cmd" => cmd));
            match self.executor.execute(target.execute_kind, cmd) {
                Ok(true) => {
                    info!(logger, "successfully executed target");
                }
                Ok(false) => {
                    return Err(ExecError::Executor {
                        description: String::from("command failed to execute"),
                    })
                }
                Err(err) => {
                    info!(logger, "failed to execute target");
                    return Err(ExecError::Executor {
                        description: err.to_string(),
                    });
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::target;
    use crate::config::{Config, TargetCfg};
    use crate::testutils::logger;

    fn get_jam(cfg: &DesugaredConfig) -> Jam {
        Jam::new(&logger::test(), Executor::new(), cfg).expect("expected no errors from parsing")
    }

    // The tests in this module involve the execution of shell commands.
    // In the ideal case, we would not need to invoke the shell and
    // use some mocking, however, the way ExecuteKind is right now
    // makes that annoying.
    mod execution {
        use crate::{executor::ExecuteKind, reconciler::Strategy, testutils::tmp::*};

        use super::*;

        #[test]
        fn executes_simple_target() {
            let expected_message = "hello world";
            let cmd = &format!("echo {expected_message}");
            let out_file = TmpFile::new();
            let cfg = Config::with_targets(vec![target::exec("blah", cmd, &out_file)]);
            let jam = get_jam(&cfg);
            jam.execute_by_shortcut(Shortcut::from_shortcut_str("b"))
                .expect("expected execution of the command to pass");
            check_file_contents(out_file, expected_message);
        }

        #[test]
        fn executes_dep() {
            let foo_cmd = "echo 'foo'";
            let bar_cmd = "echo 'bar'";
            let foo_file = TmpFile::new();
            let bar_file = TmpFile::new();
            let cfg = Config::with_targets(vec![
                target::exec("bar", bar_cmd, &bar_file),
                target::exec_deps("foo", foo_cmd, &foo_file, vec!["bar"]),
            ]);
            let jam = get_jam(&cfg);
            jam.execute_by_shortcut(Shortcut::from_shortcut_str("f"))
                .expect("expected execution of the command to pass");
            check_file_contents(foo_file, "foo");
            check_file_contents(bar_file, "bar");
        }

        #[test]
        fn executes_dep_only_when_ran_explicitly() {
            let foo_cmd = "echo 'foo'";
            let bar_cmd = "echo 'bar'";
            let foo_file = TmpFile::new();
            let bar_file = TmpFile::new();
            let cfg = Config::with_targets(vec![
                target::exec("bar", bar_cmd, &bar_file),
                target::exec_deps("foo", foo_cmd, &foo_file, vec!["bar"]),
            ]);
            let jam = get_jam(&cfg);
            jam.execute_by_shortcut(Shortcut::from_shortcut_str("b"))
                .expect("expected execution of the command to pass");
            // Foo should not have been executed, since we executed 'b', or bar, only.
            assert!(!check_file(foo_file));
            check_file_contents(bar_file, "bar");
        }

        #[test]
        fn honors_execute_kind() {
            // This is a bit bleh, cause if we had a bug and we did
            // try to write to this file, it is very possible we'd
            // fail from a permissions issue, etc. This would still
            // fail the test, so we're considering that acceptable,
            // but it would be cleaner to avoid that since a failure
            // about permissions does not directly indicate that the
            // problem is a lack of honoring of execute kinds.
            let out_file = "/tmp/blah.txt";
            let cfg = Config::with_targets(vec![TargetCfg {
                name: String::from("blah"),
                shortcut_str: None,
                help: None,
                cmd: Some(String::from("echo 'should not be ran!' > {out_file}")),
                targets: None,
                deps: None,
                execute_kind: Some(ExecuteKind::DryRun),
            }]);
            let jam = get_jam(&cfg);
            jam.execute_by_shortcut(Shortcut::from_shortcut_str("b"))
                .expect("expected execution of the command to pass");
            // Since we are executing via dry run, no command and
            // therefore no file should have been created.
            assert!(!check_file(out_file));
        }

        #[test]
        fn handles_reconciliation() {
            let expected_message = "hello world";
            let cmd = &format!("echo {expected_message}");
            let breh_file = TmpFile::new();
            let bruh_file = TmpFile::new();
            let mut cfg = Config::with_targets(vec![
                target::exec("breh", cmd, &breh_file),
                target::exec("bruh", cmd, &bruh_file),
            ]);
            cfg.options.reconciliation_strategy = Strategy::FirstNonMatch;
            let jam = get_jam(&cfg);
            // When we attempt to execute 'b-u', 'b' by itself would
            // be ambiguous.
            // 'b-u' however distinguishes 'bruh' from 'breh'. We
            // should therefore see bruh_file with the expected
            // contents, but not breh_file.
            jam.execute_by_shortcut(Shortcut::from_shortcut_str("b-u"))
                .expect("expected execution of the command to pass");
            check_file_contents(bruh_file, expected_message);
            assert!(!check_file(breh_file))
        }

        #[test]
        fn executes_by_target() {
            let expected_message = "hello world";
            let cmd = &format!("echo {expected_message}");
            let out_file = TmpFile::new();
            let cfg = Config::with_targets(vec![target::exec("blah", cmd, &out_file)]);
            let jam = get_jam(&cfg);
            jam.execute_by_target_name("blah")
                .expect("expected execution of the command to pass");
            check_file_contents(out_file, expected_message);
        }

        mod errors {
            use super::*;

            fn check_err(res: ExecResult<()>, expected_err: &str) {
                assert_eq!(
                    res.expect_err("expected execution of the command to fail")
                        .to_string(),
                    expected_err
                );
            }

            #[test]
            fn failed_reconciliation() {
                let cmd = "blah";
                let out_file_bar = TmpFile::new();
                let out_file_baz = TmpFile::new();
                let mut cfg = Config::with_targets(vec![
                    target::exec("conflict-bar", cmd, &out_file_bar),
                    target::exec("conflict-baz", cmd, &out_file_baz),
                ]);
                cfg.options.reconciliation_strategy = Strategy::Error;
                let jam = get_jam(&cfg);
                // When we attempt to execute 'b-u', 'b' by itself would
                // be ambiguous.
                // 'b-u' however distinguishes 'bruh' from 'conflict1'. We
                // should therefore see bruh_file with the expected
                // contents, but not conflict1_file.
                check_err(
                    jam.execute_by_shortcut(Shortcut::from_shortcut_str("c-b")),
                    "given shortcut 'c-b' is ambiguous (i.e. is it 'conflict-bar' or 'conflict-baz'?)",
                );
                assert!(!check_file(out_file_bar));
                assert!(!check_file(out_file_baz));
            }

            #[test]
            fn failed_execution() {
                let cmd = "idontexist";
                let out_file = TmpFile::new();
                let cfg = Config::with_targets(vec![target::exec("idontexist", cmd, &out_file)]);
                let jam = get_jam(&cfg);
                check_err(
                    jam.execute_by_shortcut(Shortcut::from_shortcut_str("i")),
                    "command failed to execute",
                );
                assert!(!check_file(out_file));
            }

            #[test]
            fn executing_cmd_that_dne() {
                let cmd = "echo 'blah'";
                let out_file = TmpFile::new();
                let cfg = Config::with_targets(vec![target::exec("idontexist", cmd, &out_file)]);
                let jam = get_jam(&cfg);
                check_err(
                    jam.execute_by_shortcut(Shortcut::from_shortcut_str("z-z-z-z-z")),
                    "no command for given shortcut 'z-z-z-z-z'",
                );
                assert!(!check_file(out_file));
            }

            #[test]
            fn execution_failure_from_dep() {
                let foo_cmd = "echo 'foo'";
                let bar_cmd = "idontexist";
                let foo_file = TmpFile::new();
                let bar_file = TmpFile::new();
                let cfg = Config::with_targets(vec![
                    target::exec("bar", bar_cmd, &bar_file),
                    target::exec_deps("foo", foo_cmd, &foo_file, vec!["bar"]),
                ]);
                let jam = get_jam(&cfg);
                check_err(
                    jam.execute_by_shortcut(Shortcut::from_shortcut_str("f")),
                    "failed to execute dependency ('foo'): command failed to execute",
                );
                // Neither file should be created. The dependency will
                // fail, which should make the dependent not even run.
                assert!(!check_file(foo_file));
                assert!(!check_file(bar_file));
            }

            #[test]
            fn shortcut_without_reconciliation_suffix() {
                let expected_message = "hello world";
                let cmd = &format!("echo {expected_message}");
                let breh_file = TmpFile::new();
                let bruh_file = TmpFile::new();
                let mut cfg = Config::with_targets(vec![
                    target::exec("breh", cmd, &breh_file),
                    target::exec("bruh", cmd, &bruh_file),
                ]);
                cfg.options.reconciliation_strategy = Strategy::FirstNonMatch;
                let jam = get_jam(&cfg);
                check_err(
                    jam.execute_by_shortcut(Shortcut::from_shortcut_str("b")),
                    "given shortcut 'b' is ambiguous (i.e. is it 'breh' or 'bruh'?)",
                );
                assert!(!check_file(bruh_file));
                assert!(!check_file(breh_file));
            }

            #[test]
            fn shortcut_without_reconciliation_suffix_three_targets() {
                let expected_message = "hello world";
                let cmd = &format!("echo {expected_message}");
                let breh_file = TmpFile::new();
                let bruh_file = TmpFile::new();
                let broh_file = TmpFile::new();
                let mut cfg = Config::with_targets(vec![
                    target::exec("breh", cmd, &breh_file),
                    target::exec("bruh", cmd, &bruh_file),
                    target::exec("broh", cmd, &broh_file),
                ]);
                cfg.options.reconciliation_strategy = Strategy::FirstNonMatch;
                let jam = get_jam(&cfg);
                check_err(
                    jam.execute_by_shortcut(Shortcut::from_shortcut_str("b")),
                    "given shortcut 'b' is ambiguous (i.e. is it 'breh' or 'bruh' or 'broh'?)",
                );
                assert!(!check_file(bruh_file));
                assert!(!check_file(breh_file));
                assert!(!check_file(broh_file));
            }
        }
    }

    #[test]
    fn test_mappings() {
        let mut cfg = Config::with_targets(vec![
            target::lone("foo"),
            target::lone("bar"),
            target::lone("baz"),
        ]);
        use crate::reconciler::Strategy;
        cfg.options.reconciliation_strategy = Strategy::FirstNonMatch;
        let jam = get_jam(&cfg);
        assert_eq!(
            jam.mappings().expect("failed to get mappings"),
            vec![
                (Shortcut::from_shortcut_str("b-r"), "bar"),
                (Shortcut::from_shortcut_str("b-z"), "baz"),
                (Shortcut::from_shortcut_str("f"), "foo")
            ]
        );
    }
}
