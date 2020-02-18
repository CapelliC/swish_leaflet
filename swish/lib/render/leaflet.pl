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
:- use_module(library(option)).
:- use_module(library(debug)).

:- use_module(config(plugin_leaflet)).
:- register_renderer(leaflet, "Render geomaps with Leaflet").

/** <module> SWISH Leaflet based renderer

Render geomap data as an interactive Leaflet map.
*/

%%  term_rendering(+Term, +Vars, +Options)//
%
%   Renders Term as a Leaflet map.
%   I'm pleased to see how easy is to pass JSON to
%   js_script//1, using quasi-quotation
%
term_rendering(Term,_Vars,Options) -->
  {
    select_option(width(Width),Options,Options1,'400px'),
    select_option(height(Height),Options1,Options2,'400px'),
    select_option(backgroundColor(BackgroundColor),Options2,Options3,ivory),

    LlUrl='/plugin/leaflet/leaflet.js'
  },
  html(div([ class(['render-leaflet']),
        'data-render'('Leaflet geomap')
      ],
      [ \js_script({|javascript(LlUrl,Width,Height,BackgroundColor)||
(function() {
  var trace = console.log
  if ($.ajaxScript) {

    var l_cont = $.ajaxScript.parent()

    // fix div size
    $(l_cont).css('height', Height)
    $(l_cont).css('width', Width)

    // just to inspect the render area
    $(l_cont).css('background-color', BackgroundColor)

    require([LlUrl], function(cytoscape) {
trace('running leaflet', Elements,Style,Layout)
      try {
        var provider = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
        var mymap = L.map('mapid')..setView([51.505, -0.09], 13)
        
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
        $(l_cont).html(e)
      }
    })
  }
})();
    |})
  ])).
