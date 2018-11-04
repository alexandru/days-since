# days-since

A simple program that calculates how many days have passed since a
certain specified date:

```
$ days-since 2018-07-12

Days:    115
Weeks:  16.4
Months:  3.8
```

## How to Build

Install [Stack](https://docs.haskellstack.org/). You can do so via:

```
curl -sSL https://get.haskellstack.org/ | sh
```

Then in the project's directory:

```
stack build
```

This will output an executable in a directory like:

```
.stack-work/install/x86_64-osx/lts-12.16/8.4.4/bin
```

Copy that somewhere in your path.
