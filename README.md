# Library Runtime

A lightweight erlang runtime. 
No processes are started by the init process.
No modules other than a small see runtime are loaded.
You define a module with a `main` function which is called.
Upon completion of the main funtion the process stops.

Such a main function can start supervision trees and other long lived processes.

The aim of this project is to write a runtime as library.
I.e. you explicitly require all modules and start all processes that your program needs.
All configuration is code in the main loop.

i.e. in the future
```rust
import gleam/logger
import gleam/ssl

pub fn main() {
  assert Ok(pid) = logger.start(config)
  assert Ok(ssl) = ssl.start(config)
  // etc
}
```

## Usage

Ergonomics of this can definetly be improved. But for now the steps are as follows

1. Write a Gleam program in `/src`
2. run `./build`
3. run `./run name` (where name is the name of your module that has the main function)


## Notes

#### Counting loaded modules and processes

```
code:all_loaded().
length(processes()).
length(registered()).
```

#### Erlang System Principles
https://erlang.org/doc/system_principles/system_principles.html#default_boot_scripts

Explains boot scripts, and enumerates default boot scripts. There IS a boot script without SASL.


#### Erlang Runtime options

http://erlang.org/doc/man/erl.html

`-s module` Tries to start with a call to `module:start()`
`-s module function` Tries to start with a call to `module:function()`

More than one `-s` can be specified. To start a program that stops. use.
`erl -s my_module main -s init stop -noshell`


`-extra` everything after extra is considered plain arguments and can be loaded using `init:get_plain_arguments()`

`-r` works the same as `-s` except with this comment in the docs

```
Because of the limited length of atoms, it is recommended to use -run instead.
```

##### Mode

interactive/embedded

Default mode is interactive, it loads code files on demand. `code:get_mode().` allows you to see which it is at runtime.

#### Erlang preloaded source
https://github.com/erlang/otp/tree/master/erts/preloaded/src
