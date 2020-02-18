/*  File:    install.pl
    Author:  Carlo,,,
    Created: Jan 22 2020
    Purpose: copy files and folders to swish target directory
*/

:- module(install, []).

:- initialization copy_resources.

copy_resources :-

    % adjust this path to your SWISH installation directory
    SwishRoot = '~/swish',

    expand_file_name(SwishRoot, [SwishDir]),
    assertion(exists_directory(SwishDir)),

    module_property(install, file(Install)),
    file_directory_name(Install, InstallDir),

    maplist(copy_resource(SwishDir, InstallDir), [
                'config-enabled/web/plugin/leaflet/leaflet.js',
                'config-enabled/web/plugin/leaflet/leaflet.css',
                'config-enabled/web/plugin/leaflet/images/layers.png',
                'config-enabled/web/plugin/leaflet/images/layers-2x.png',
                'config-enabled/web/plugin/leaflet/images/marker-icon.png',
                'config-enabled/web/plugin/leaflet/images/marker-icon-2x.png',
                'config-enabled/web/plugin/leaflet/images/marker-shadow.png',
                'config-enabled/plugin_leaflet.pl',
                'lib/render/leaflet.pl',
                'examples/test_leaflet.swinb'
            ]),

    path(SwishDir, 'swish.pl', SwishPl),
    read_file_to_string(SwishPl, S, []),
    path(InstallDir, 'swish/swish_append.pl', Swish_append),
    read_file_to_string(Swish_append, T, []),
    (   sub_string(S, _,_,_, T)
    ->  writeln('leaflet already installed?')
    ;   open(SwishPl, write, SP),
        format(SP, '~s~n~s', [S, T]),
        close(SP)
    ),

    path(SwishDir, 'examples/swish_tutorials.swinb', ExTutPath),

    open(ExTutPath, append, S_ExTutPath),
    format(S_ExTutPath, '~n~s~n', [`
<div class="notebook">
<div class="nb-cell markdown">
---
These notebooks show the basics of Leaflet renderer

  - [Test installation](example/test_leaflet.swinb)

</div>
</div>
`]),
    close(S_ExTutPath),
    
    writeln('install done.'),
    halt.

copy_resource(SwishDir, InstallDir, Relative) :-
    path(InstallDir, swish/Relative, SrcPath),
    path(SwishDir, Relative, DstPath),
    file_directory_name(DstPath, DstDir),
    make_directory_path(DstDir),
    copy_file(SrcPath, DstPath).

path(D, F, P) :-
    format(atom(P), '~w/~w', [D, F]).
