# tidymusic
*R package for tidyverse-style music parsing and analysis*

This is the very beginning of the `tidymusic` package. It's not an actual package yet, just a set of basic functions (in `functions.R`) and a test analysis file (`import_parse_analyze.R`), along with a couple sample MusicXML melody files for testing.

Currently, this works on the test files — which are MELODY ONLY — or *tidy* data tables with one pitch per row, and with pitch-class and octave information in columns, and performs basic key-based and interval-based pitch analysis.

Lots more features and robustness is coming in the future: chords, multiple parts, rhythm, post-tonal calculations, functional harmonic analysis, key-finding, etc.

Testing and contributions in the form of pull requests are welcome. Please maintain the tidy-data/tidyverse focus in any contributions. I'm particularly interested in help parsing MusicXML files into usable tidy formats, particularly for files with multiple parts. I'm also focused on _analysis_ here, rather than composition or converting between different file formats.

Enjoy!
