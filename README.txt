            _ _____
           | |  __ \
   ___ _ __| | |__) | __ ___  ___ ___
  / _ \ '__| |  ___/ '__/ _ \/ __/ __|
 |  __/ |  | | |   | | |  __/\__ \__ \
  \___|_|  |_|_|   |_|  \___||___/___/


========
erlPress
========

The erlPress print-media page layout and typesetting system modifies and extends the Hugh Watkins fork of the Erlang Erlguten library originally developed by Joe Armstrong.

https://github.com/hwatkins/erlguten

As of Version .01, ErlPress is a work-in-progress with many deficiencies and rough-edges. You might consider it a PDF generation test bed. The MIT license encourages evolution toward world-class functionality and performance including:

* print project management
* page design and layout
* copyfitting
* typesetting
* bitmap and vector scaling and placement

We imagine two versions:

1. Embedded - based on pre-defined document/page layout templates, markdown copy, and Erlang functions

2. Web-based GUI along the lines of Scribus


===========
Directories
===========
copy     - experiments with markdown
elements - experiments with PDF geometric elements
erlguten - useful erlguten modules
fonts    - fonts from erlguten
layout   - page layout structures and tests
lib      - library functions
media    - paper and digital media dimensions and conversions
parser   - experiments with markdown parser
paste_up - copyfitting functions
typespec - type specification functions

Interesting stuff to explore:

. GEOMETRIC ELEMENTS

See src/elements

. PAGE LAYOUT

See src/layout

. Structures

ep_box.erl
ep_panel.erl
ep_page.erl

. Functions

ep_block.erl - copyfitting functions factored from erlguten
ep_grid.erl

. Example page grids

ep_page_grids.erl



==============
3rd Party Libs
==============

. CMARK

CommonMark parsing and rendering library and program in C

Linux  : https://github.com/commonmark/cmark
Windows: https://github.com/commonmark/cmark
macOS  : brew install cmark
