### TEST FUNCTIONS ###

pitch_class_integer('C+')
generic_pitch_class('C+')
pitch_class_interval('C#', 'A')
pitch_class_interval('A', 'C#')
pitch_class_interval('bb', 'a+')
pitch_class_interval('c-', 'c#')
generic_pitch_class_interval('C#', 'A')
generic_pitch_class_interval('A', 'C#')
generic_pitch_class_interval('bb', 'a+')
generic_pitch_class_interval('c-', 'c#')
generic_pitch_class('c#')
diatonic_scale_degree('d', 'c')
diatonic_scale_degree('es', 'c')
diatonic_scale_degree('bb', 'a#')
diatonic_scale_degree('fis', 'C-')
pitch_interval('a', 4, 'c', 4)
pitch_interval('e', 3, 'd', 4)
pitch_interval('c#', 4, 'b', 3)
pitch_interval('a', 3, 'fis', 4)
pitch_interval('fis', 6, 'ges', 6)



### TEST MUSICXML MELODY ###

song <- read_music_xml_melody('DesBachesWiegenliedMusic.xml')


song_analysis <- song %>%
  mutate(key = song$pitch_class[length(song$pitch_class)],
         scale_degree = mapply(diatonic_scale_degree, pitch_class, key),
         generic_pc = mapply(generic_pitch_class, pitch_class),
         pc_integer = mapply(pitch_class_integer, pitch_class),
         diatonic_int = mapply(diatonic_interval, lag(pitch_class), lag(octave), pitch_class, octave),
         pitch_interval = mapply(pitch_interval, lag(pitch_class), lag(octave), pitch_class, octave),
         pc_int = mapply(pitch_class_interval, lag(pitch_class), pitch_class),
         int_class = mapply(interval_class, lag(pitch_class), pitch_class))

song_analysis %>%
  filter(!is.na(pitch_interval)) %>%
  count(pitch_interval, sort = TRUE)

song_analysis %>%
  filter(!is.na(diatonic_int)) %>%
  count(diatonic_int, sort = TRUE)

song_analysis %>%
  filter(!is.na(pitch_class)) %>%
  count(pitch_class, sort = TRUE)



