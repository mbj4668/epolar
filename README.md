# EPolar

Erlang library for handling speed predictions for sailing boats (often
called the boat's "polar" or "polar table", from the common practice
of visualizing the speed predicition in a polar diagram).

Polar data is represented as `epolar:polar()`, and can be manipulated
and queried using the functions in `epolar`.

The function `epolar_orc:read_sylk()` can be used to read polar data in
the form of ORC's SYLK file format (".slk" files).
