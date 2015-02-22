# Daisy

A library to make clojure
[Components](https://github.com/stuartsierra/component) easier to use.

## Usage

FIXME

## Rationale

The component library is a large step forward in the organization of
clojure libraries, but it is not perfect.

Here are the usability problems I have found with it.

Creating the component record in one place and then listing its
dependencies in another seems wrong.

Looking at the component record, there is no way to tell the
difference between fields that should be passed in the constructor,
those that should be injected, and those that will be set by the
component itself in its start method.

Making start and stop methods idempotent seems boilerplaty.

Want to add prints? eg: "Staring webserver...", "Stopping
webserver...". There is no obvious way to do this without adding
boilerplate to each component. Maybe it would be possible to add an
alternative `start-system` function that prints what it's doing?

Exceptions during startup leave the system in a half-started
state, but with nothing stored in the system map (at least in the
"standard" setup, using alter-var-root). This sucks when you have a
port bound, but no access to the server to shut it down - you have to
just kill the repl and restart.

System map is hard to inspect. Probably need a custom-build
pretty-printer that does not re-print referenced components.

Missing configs can be hard to track down, I have found. Well, in one
case so far anyway - I was passing a nil port to jetty 9. Actually, I
think the problem was more that I was not passing `:join? false`, so
my system start was hanging, then never associng, so it was running,
holding on to the port, and I had no handle on it to stop it. So never
mind. But I still feel like I want a better way to to declare required
configs.


## License

Copyright © 2015 Chris Perkins

Distributed under the MIT License

