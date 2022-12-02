# Gradualizer Changelog

Changelog generated with [`github-changelog`](https://github.com/cfpb/github-changelog),
the following command for each pair of tags, and a bit of manual editing:

```
changelog -m --branch master josefs gradualizer 0.1.3 0.2.0 >> CHANGELOG.md
```


## 2022-12-02 - [0.2.0](https://github.com/josefs/Gradualizer/compare/0.1.3...0.2.0)

Release highlights:

- Use ExDoc [#456](https://github.com/josefs/gradualizer/pull/456)
- Function intersections [#461](https://github.com/josefs/gradualizer/pull/461)
- Fix more infinite loops [#458](https://github.com/josefs/gradualizer/pull/458)
- Add `gradualizer_tracer` for efficient troubleshooting [#454](https://github.com/josefs/gradualizer/pull/454)
- Enable Dialyzer and ETC cross checks [#429](https://github.com/josefs/gradualizer/pull/429)
- Fix some self-gradualization errors [#408](https://github.com/josefs/gradualizer/pull/408)
- Point README at [Gradient](https://github.com/esl/gradient) instead of discontinued Gradualixir [#396](https://github.com/josefs/gradualizer/pull/396)
- Property based tests to find bugs [#363](https://github.com/josefs/gradualizer/pull/363)
- Add a GitHub Actions CI workflow [#343](https://github.com/josefs/gradualizer/pull/343)
- Non-trivial sum type exhaustiveness checking [#330](https://github.com/josefs/gradualizer/pull/330)

Full list of merged PRs:

- Add known problem for unsupported maybe expression [#489](https://github.com/josefs/gradualizer/pull/489)
- Override filename specs [#487](https://github.com/josefs/gradualizer/pull/487)
- Use ExDoc [#456](https://github.com/josefs/gradualizer/pull/456)
- Define some shell utils for interactive work with Gradualizer [#455](https://github.com/josefs/gradualizer/pull/455)
- Move test from `test/known_problems/should_pass/intersection.erl` to `test/should_pass/intersection_pass.erl` [#486](https://github.com/josefs/gradualizer/pull/486)
- Expand `erlang:'++'` function specs [#485](https://github.com/josefs/gradualizer/pull/485)
- Function intersections [#461](https://github.com/josefs/gradualizer/pull/461)
- fix GitHub reference in rebar3 example README [#482](https://github.com/josefs/gradualizer/pull/482)
- Override the spec of `erlang:error/3` too [#481](https://github.com/josefs/gradualizer/pull/481)
- Fix `erlang:error/2` spec [#478](https://github.com/josefs/gradualizer/pull/478)
- add xref ignores to header file defining `::`/`:::` [#475](https://github.com/josefs/gradualizer/pull/475)
- Expand user types in record unions [#469](https://github.com/josefs/gradualizer/pull/469)
- Handle `non_neg_integer() + pos_integer() :: pos_integer()` properly [#465](https://github.com/josefs/gradualizer/pull/465)
- Fix integer user type alias [#463](https://github.com/josefs/gradualizer/pull/463)
- Add `install-asdf` rule to Makefile [#459](https://github.com/josefs/gradualizer/pull/459)
- Housekeeping [#460](https://github.com/josefs/gradualizer/pull/460)
- Fix more infinite loops [#458](https://github.com/josefs/gradualizer/pull/458)
- Fix map type inference [#457](https://github.com/josefs/gradualizer/pull/457)
- Add `gradualizer_tracer` for efficient troubleshooting [#454](https://github.com/josefs/gradualizer/pull/454)
- Housekeeping [#453](https://github.com/josefs/gradualizer/pull/453)
- Fix some infinite loops [#452](https://github.com/josefs/gradualizer/pull/452)
- Fix crash when a record union contains `any()` [#447](https://github.com/josefs/gradualizer/pull/447)
- `merge_with` in OTP 24! [#335](https://github.com/josefs/gradualizer/pull/335)
- Add specs for `++/2` and `--/2` [#440](https://github.com/josefs/gradualizer/pull/440)
- Propagate var binds across list comprehension filters [#444](https://github.com/josefs/gradualizer/pull/444)
- Remove `.envrc` [#443](https://github.com/josefs/gradualizer/pull/443)
- Fix more self check errors [#441](https://github.com/josefs/gradualizer/pull/441)
- Fix `gradualizer_int:int_type_to_range/1` crash [#439](https://github.com/josefs/gradualizer/pull/439)
- Fix record field access type check crash [#436](https://github.com/josefs/gradualizer/pull/436)
- Add examples of list comprehension generators being `none()` [#434](https://github.com/josefs/gradualizer/pull/434)
- Export function in tests to fix a Dialyzer error [#431](https://github.com/josefs/gradualizer/pull/431)
- Fix Dialyzer warning in tests [#430](https://github.com/josefs/gradualizer/pull/430)
- Enable Dialyzer and ETC cross checks [#429](https://github.com/josefs/gradualizer/pull/429)
- Another batch of type error fixes [#427](https://github.com/josefs/gradualizer/pull/427)
- Simplify `refinable(#{})` [#424](https://github.com/josefs/gradualizer/pull/424)
- Make an empty `map()` refinable [#420](https://github.com/josefs/gradualizer/pull/420)
- Make `binary()`, aka `String.t()` in Elixir, refinable [#392](https://github.com/josefs/gradualizer/pull/392)
- Fix false nonexhaustive record patterns warning [#419](https://github.com/josefs/gradualizer/pull/419)
- Fix some self-gradualization errors [#408](https://github.com/josefs/gradualizer/pull/408)
- Redefine `top()` to `none()` to clean up remaining compilation warnings [#402](https://github.com/josefs/gradualizer/pull/402)
- Always throw `call_undef` with raw module, function, and arity [#415](https://github.com/josefs/gradualizer/pull/415)
- Fix list exhaustiveness checking regressions [#404](https://github.com/josefs/gradualizer/pull/404)
- Salvage exhaustiveness checking improvements [#403](https://github.com/josefs/gradualizer/pull/403)
- Check exhaustiveness argument-wise [#391](https://github.com/josefs/gradualizer/pull/391)
- Point README at [Gradient](https://github.com/esl/gradient) instead of discontinued Gradualixir [#396](https://github.com/josefs/gradualizer/pull/396)
- Fix variable bind before a record pattern match in function head [#397](https://github.com/josefs/gradualizer/pull/397)
- Fix `pp_type({var, ..., _})` [#401](https://github.com/josefs/gradualizer/pull/401)
- Fix for map pattern "doesn't have type any()" warning [#389](https://github.com/josefs/gradualizer/pull/389)
- Temporary measure: break cycles by timing out [#383](https://github.com/josefs/gradualizer/pull/383)
- Recover position information for `undef`/`not_exported` type errors [#384](https://github.com/josefs/gradualizer/pull/384)
- Provide the git commit sha when asked for `--version` in CLI [#385](https://github.com/josefs/gradualizer/pull/385)
- Fix typos [#377](https://github.com/josefs/gradualizer/pull/377)
- Call `remove_pos` only where a type comes into the system [#375](https://github.com/josefs/gradualizer/pull/375)
- Property based tests to find bugs [#363](https://github.com/josefs/gradualizer/pull/363)
- Allow giving include path to erl files imported into db [#344](https://github.com/josefs/gradualizer/pull/344)
- Add a GitHub Actions CI workflow [#343](https://github.com/josefs/gradualizer/pull/343)
- Non-trivial sum type exhaustiveness checking [#330](https://github.com/josefs/gradualizer/pull/330)
- Extract type env from typechecker.erl and convert it to a map [#333](https://github.com/josefs/gradualizer/pull/333)


## 2021-02-18 - [0.1.3](https://github.com/josefs/Gradualizer/compare/0.1.2...0.1.3)

- Record update missing fields fix [#314](https://github.com/josefs/gradualizer/pull/314)
- Any type fix within complex patterns [#315](https://github.com/josefs/gradualizer/pull/315)
- should_fail/intersection_with_any - one more example [#305](https://github.com/josefs/gradualizer/pull/305)
- Compile test data before gradualizing  [#303](https://github.com/josefs/gradualizer/pull/303)


## 2020-11-06 - [0.1.2](https://github.com/josefs/Gradualizer/compare/0.1.1...0.1.2)

- Expand dirs in rebar so exclusion works [#296](https://github.com/josefs/gradualizer/pull/296)


## 2020-11-02 - [0.1.1](https://github.com/josefs/Gradualizer/compare/0.1.0...0.1.1)

- Warning and shadowing [#293](https://github.com/josefs/gradualizer/pull/293)


## 2020-10-19 - [0.1.0](https://github.com/josefs/Gradualizer/compare/0.0.0...0.1.0)

- Record refinement [#271](https://github.com/josefs/gradualizer/pull/271)
- Bit syntax integer signedness [#283](https://github.com/josefs/gradualizer/pull/283)
- More tests for opaque types [#277](https://github.com/josefs/gradualizer/pull/277)
- Allow type refinement in the presence of matching opaque types. [#276](https://github.com/josefs/gradualizer/pull/276)
- Increase coverage [#260](https://github.com/josefs/gradualizer/pull/260)
- Normalize annotated types [#228](https://github.com/josefs/gradualizer/pull/228)
- Returning incorrectly typed map should fail [#238](https://github.com/josefs/gradualizer/pull/238)
- Accept var in update_map_type [#237](https://github.com/josefs/gradualizer/pull/237)
- Add failing named_record_arg/1 test [#225](https://github.com/josefs/gradualizer/pull/225)
- Fix broken CLI features and add CLI tests [#189](https://github.com/josefs/gradualizer/pull/189)
