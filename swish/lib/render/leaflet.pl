/*  Part of SWISH

    Author:        Carlo Capelli
    E-mail:        cc.carlo.cap@gmail.com

    Copyright (c)  2019, Carlo Capelli
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

:- module(swish_render_leaflet,
    [term_rendering//3 % +Term, +Vars, +Options
    ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module('../render').
:- use_module(library(debug)).

:- use_module(config(plugin_leaflet)).
:- register_renderer(leaflet, "Render geomaps with Leaflet").

/** <module> SWISH Leaflet based renderer

Render geomap data as an interactive Leaflet map.
*/

options_defaults(OptionsIn, Options) :-
  maplist({OptionsIn}/[Opt]>>(
      Opt =.. [Key,Def,Var],
      Vin =.. [Key,Var],
      ( memberchk(Vin, OptionsIn) -> true ; Var = Def )
    ), Options).

%%  term_rendering(+Term, +Vars, +Options)//
%
%   Renders Term as a Leaflet map.
%   I'm pleased to see how easy is to pass JSON to
%   js_script//1, using quasi-quotation
%
term_rendering(Term,Vars,OptionsIn) -->
  {
    Options = [
      id(mapid,Id),
      width('400px',Width),
      height('400px',Height),
      backgroundColor(ivory,BackgroundColor)
    ],
    options_defaults(OptionsIn, Options),
    debug(leaflet, 'Term:~w Vars:~w Options:~w', [Term,Vars,Options])
  },
  html(div([ id(Id), class(['render-leaflet']),
        'data-render'('Leaflet geomap')
      ],
      [ \js_script({|javascript(Id,Width,Height,BackgroundColor)||
(function() {
  var trace = console.log
  if ($.ajaxScript) {

    var l_cont = $.ajaxScript.parent()
    var $c = $(l_cont)
    // fix div size
    if (Height == 'auto') {
      $c.css('height', $c.css('width'))
    } else {
      $c.css('height', Height)
    }
    if (Width == 'auto') {
      $c.css('width', $c.css('height'))
    } else {
      $c.css('width', Width)
    }

    // just to inspect the render area
    $c.css('background-color', BackgroundColor)

    require(['/plugin/leaflet/leaflet.js'], function(L) {
trace('leaflet instance', L)
      try {
        var provider = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
        var mymap = L.map(Id).setView([0, 0], 1)

        L.tileLayer(provider, {
            attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
            maxZoom: 18,
            id: 'mapid-layer',
            tileSize: 512,
            zoomOffset: -1,
            //accessToken: 'your.mapbox.access.token'
        }).addTo(mymap)

      }
      catch(e) {
        $c.html(e)
      }
    })
  }
})();
    |})
  ])).
