rebar3_eep48
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_eep48, {git, "https://host/user/rebar3_eep48.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_eep48
    ===> Fetching rebar3_eep48
    ===> Compiling rebar3_eep48
    <Plugin Output>
