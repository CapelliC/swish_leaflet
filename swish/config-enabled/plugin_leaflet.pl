/*  Part of SWISH

    Author:        Carlo Capelli
    E-mail:        cc.carlo.cap@gmail.com

    Copyright (c)  2020, Carlo Capelli
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(plugin_leaflet,
    [
    ]).

/** <module> Provide Leaflet as a plugin

Adapted from plugin_slider.pl.

Currently, a plugin is a dict that may provide the following fields:

  - name:Name
  Name of the plugin.  Currently ignored.
  - js:JavaScript
  Either a single JavaScript URL or a list of JavaScript URLS.  Each
  URL can be a term that is handed to http_absolute_location/3.  Notably
  the `plugin` _alias_ allows loading files from `web/plugin` in
  `config-enabled` or `config-available`.  The JavaScript files are
  loaded after loading SWISH and before instantiating the SWISH
  element using RequireJS.
  - css:Styles
  Uses the same URL conventions as for `js`.  Each style is loaded by
  adding a `link` element to the document `head`.  These styles are
  added logically _after_ the built-in styles.
*/

:- multifile
    swish_config:web_plugin/1.

swish_config:web_plugin(
    json{name: cytoscapejs,
         js: plugin('leaflet/leaflet.js'),
         css: plugin('leaflet/leaflet.css')
        }).
