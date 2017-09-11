Quick Start (success)

  $ cd "$TESTDIR/.."
  $ stack ghci --ghci-options="-v0" test/quick-start.hs <<EOM
  > main
  > Pat
  > 21
  > hunter2
  > y
  > EOM
  * (glob)
  * (glob)
  * (glob)
  Name: Age: Password: Subscribe: (y/n)? Right (Registration {name = "Pat", age = 21, password = "hunter2", subscribe = True})

Quick Start (failure)

  $ cd "$TESTDIR/.."
  $ stack ghci --ghci-options="-v0" test/quick-start.hs <<EOM
  > main
  > Pat
  > apple
  > EOM
  * (glob)
  * (glob)
  * (glob)
  Name: Age: Left "Prelude.read: no parse"
