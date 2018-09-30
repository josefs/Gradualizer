# Gradualizer plugin for rebar3

To run Gradualizer from rebar3, add it as a plugin in your `rebar.config`:
```Erlang
{plugins, [
  {gradualizer, {git, "git://github.com/josefs/Gradualizer.git", {branch, "master"}}}
]}.
```

# Options

Gradualizer checks every source file in your app(s), unless configured not to.  
Configuration is read from `gradualizer_opts` in `rebar.config` which
should be a `proplists:proplist()`.  
The following options are supported:  

## include

type: `[filelib Wildcard]`

Files to type check - Gradualizer includes every `.erl` file in the source directory if undefined

## exclude

type: `[filelib Wildcard]`

Files to not type check. Subtracts the list of included files

## stop_on_first_error

type: `boolean()`

if 'true' stop type checking at the first error, if 'false' continue checking all functions in the given file and all files in the given directory
