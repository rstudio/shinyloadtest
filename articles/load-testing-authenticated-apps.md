# Load Testing Authenticated Apps

`shinyloadtest` supports load testing applications served by [Posit
Connect](https://posit.co/products/enterprise/connect/) with the
following authentication configurations:

- Connect
  - Password
  - LDAP and Active Directory
  - PAM

## Recording

[`shinyloadtest::record_session`](https://rstudio.github.io/shinyloadtest/reference/record_session.md)
will automatically detect when an application is protected using a
supported authentication scheme and will prompt for a username and
password.

If `record_session` is called from within an RStudio IDE session, two UI
dialogs will appear. If it’s called from an R session in a command-line
setting, prompts will appear on standard output.

The supplied username and password are used for the duration of the
recording to interact with the application, but these credentials are
not stored in the recording file.

## Load testing

Because credentials are not stored in recordings, a username and
password or Posit Connect API Key must be supplied to `shinycannon` in
order to load test authenticated applications.

`shinycannon` does not have `--user`, `--password`, or
`--connect-api-key` arguments for security reasons, so that these
strings will not appear in a process list. Instead, the
`SHINYCANNON_USER`, `SHINYCANNON_PASS`, and
`SHINYCANNON_CONNECT_API_KEY` environment variables must be set.
`SHINYCANNON_CONNECT_API_KEY` will take preference over
`SHINYCANNON_USER` and `SHINYCANNON_PASS`.

On Linux and macOS, you can disable history for any command that starts
with a space. If using a `bash`:

    $ # Disable history for any command that starts with a space
    $ HISTCONTROL=ignoreboth
    $  export SHINYCANNON_USER=myUser
    $  export SHINYCANNON_PASS=myPass
    $
    $ shinycannon recording.log https://shinyapp.example.com/ --workers 5 --loaded-duration-minutes 2 --output-dir run1

And if using `zsh`:

    $ # Disable history for any command that starts with a space
    $ setopt HIST_IGNORE_SPACE
    $  export SHINYCANNON_CONNECT_API_KEY=$CONNECT_API_KEY
    $
    $ shinycannon connect_recording.log https://connect.example.com/content/545/ --workers 5 --loaded-duration-minutes 2 --output-dir run2
