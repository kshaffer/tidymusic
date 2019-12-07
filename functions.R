### IMPORT ###
library(tidyverse)
library(xml2)

### FUNCTIONS ###

read_music_xml_melody <- function(xml_file) {
  file_opened <- read_xml(xml_file)
  
  notes <- file_opened %>%
    xml_find_all('//part') %>%
    xml_find_all('//note') %>%
    xml_find_all('//pitch')
  
  i = 1
  
  for (note in notes) {
    note_parsed <- tibble(
      key = note %>%
        xml_children() %>%
        xml_name(),
      value = note %>%
        xml_children() %>%
        xml_text()
    ) %>%
      spread(key, value)
    
    if (i == 1) {
      notes_parsed <- note_parsed
    } else {
      notes_parsed <- notes_parsed %>%
        bind_rows(note_parsed)
    }
    i <- i + 1
  }
  
  notes_parsed[is.na(notes_parsed$alter),]$alter <- ''
  
  notes_parsed <- notes_parsed %>%
    mutate(alter = str_replace_all(alter, '1', '#'),
           alter = str_replace_all(alter, '0', ''),
           alter = str_replace_all(alter, '-1', 'b'),
           pitch_class = str_c(tolower(step), alter),
           octave = as.numeric(octave)) %>%
    select(pitch_class, octave)
  
  return(notes_parsed)
}


pitch_class_integer <- function(pitch) {
  if (tolower(pitch) %in% c('d', 're', '2')) {
    return(2)
  } else if (tolower(pitch) %in% c('g', 'sol', '7')) {
    return(7)
  } else if (tolower(pitch) %in% c('a', 'la', '9')) {
    return(9)
  } else if (tolower(pitch) %in% c('c', 'do', '0')) {
    return(0)
  } else if (tolower(pitch) %in% c('e', 'mi', '4')) {
    return(4)
  } else if (tolower(pitch) %in% c('f', 'fa', '5')) {
    return(5)
  } else if (tolower(pitch) %in% c('b', 'ti', '11')) {
    return(11)
  } else if (tolower(pitch) %in% c('bes', 'bb', 'b-', 'te', '10',
                                   'ais', 'a#', 'a+', 'li')) {
    return(10)
  } else if (tolower(pitch) %in% c('fis', 'f#', 'f+', 'fi', '6',
                                   'ges', 'gb', 'g-', 'se')) {
    return(6)
  } else if (tolower(pitch) %in% c('ees', 'es', 'eb', 'e-', 'me', '3',
                                   'dis', 'd#', 'd+', 'ri')) {
    return(3)
  } else if (tolower(pitch) %in% c('cis', 'c#', 'c+', 'di', '1',
                                   'des', 'db', 'd-', 'ra')) {
    return(1)
  } else if (tolower(pitch) %in% c('aes', 'ab', 'a-', 'le', '8',
                                   'gis', 'g#', 'g+', 'si')) {
    return(8)
  } else if (tolower(pitch) %in% c('bis', 'b#', 'b+')) {
    return(0)
  } else if (tolower(pitch) %in% c('fes', 'fb', 'f-')) {
    return(4)
  } else if (tolower(pitch) %in% c('ces', 'cb', 'c-')) {
    return(11)
  } else if (tolower(pitch) %in% c('eis', 'e#', 'e+')) {
    return(11)
  }
}

pitch_class_interval <- function(starting_pitch_class, ending_pitch_class) {
  if(!is.na(starting_pitch_class) & !is.na(ending_pitch_class)) {
    return((pitch_class_integer(ending_pitch_class) - pitch_class_integer(starting_pitch_class)) %% 12)
  } else {
    return(NA)
  }
}

interval_class <- function(starting_pitch_class, ending_pitch_class) {
  if(!is.na(starting_pitch_class) & !is.na(ending_pitch_class)) {
    pci <- pitch_class_interval(starting_pitch_class, ending_pitch_class)
    if (pci <= 6) {
      return(pci)
    } else {
      return(12 - pci)
    }
  } else {
    return(NA)
  }
}

generic_pitch_class <- function(pitch) {
  return(tolower(unlist(strsplit(pitch, ''))[1]))
}

generic_pitch_class_interval <- function(starting_pitch_class, ending_pitch_class) {
  gpc <- c('a', 'b', 'c', 'd', 'e', 'f', 'g')
  return((match(generic_pitch_class(ending_pitch_class), gpc) - 
            match(generic_pitch_class(starting_pitch_class), gpc)) %% 7 + 1)
}

absolute_pitch <- function(pitch, octave) {
  pc <- pitch_class_integer(pitch)
  return(pc + (octave * 12))
}

generic_pitch <- function(pitch, octave) {
  gpc <- generic_pitch_class_interval('c', pitch)
  return(gpc + (octave * 7))
}

pitch_interval <- function(starting_pitch_class, starting_octave, ending_pitch_class, ending_octave) {
  if(!is.na(starting_pitch_class) & !is.na(ending_pitch_class) &
     !is.na(starting_octave) & !is.na(ending_octave)) {
    return(absolute_pitch(ending_pitch_class, ending_octave) -
           absolute_pitch(starting_pitch_class, starting_octave))
  } else {
    return(NA)
  }
}

diatonic_scale_degree <- function(pitch, key) {
  dsd <- (generic_pitch_class_interval(key, pitch)) %% 7
  if (dsd == 0) {
    return(7)
  } else {
    return(dsd)
  }
}

generic_diatonic_interval <- function(starting_pitch_class, starting_octave, ending_pitch_class, ending_octave) {
  if(!is.na(starting_pitch_class) & !is.na(ending_pitch_class) &
     !is.na(starting_octave) & !is.na(ending_octave)) {
    diff <- generic_pitch(ending_pitch_class, ending_octave) - 
      generic_pitch(starting_pitch_class, starting_octave)
    if (diff >= 0) {
      return(diff + 1)
    } else {
      return(diff - 1)
    }
  } else {
    return(NA)
  }
}

diatonic_interval <- function(starting_pitch_class, starting_octave, ending_pitch_class, ending_octave) {
  if(!is.na(starting_pitch_class) & !is.na(ending_pitch_class) &
     !is.na(starting_octave) & !is.na(ending_octave)) {
    pi <- pitch_interval(starting_pitch_class, starting_octave, ending_pitch_class, ending_octave)
    gdi <- generic_diatonic_interval(starting_pitch_class, starting_octave, ending_pitch_class, ending_octave) - 1

    if (pi >= 0) {
       pi <- pi %% 12
    } else {
      pi <- -(12 - (pi %% 12))
    }
    
    if (gdi > 6 & (gdi %% 7) == 0) {
      gdi <- 8
    } else if (gdi < -7 & ((gdi + 2) %% 7) == 0) {
      gdi <- -8
    } else if (gdi >= -1) {
      gdi <- (gdi %% 7) + 1
    } else {
      gdi <- -(7 - ((gdi + 1) %% 7))
    }
    
    if (gdi == 1 & pi == -1) {
      return('d1')
    } else if (gdi == 1 & pi == 0) {
      return('P1')
    } else if (gdi == 1 & pi == 1) {
      return('A1')
    } else if (gdi == 2 & pi == 0) {
      return('d2')
    } else if (gdi == 2 & pi == 1) {
      return('m2')
    } else if (gdi == 2 & pi == 2) {
      return('M2')
    } else if (gdi == 2 & pi == 3) {
      return('A2')
    } else if (gdi == 3 & pi == 2) {
      return('d3')
    } else if (gdi == 3 & pi == 3) {
      return('m3')
    } else if (gdi == 3 & pi == 4) {
      return('M3')
    } else if (gdi == 3 & pi == 5) {
      return('A3')
    } else if (gdi == 4 & pi == 4) {
      return('d4')
    } else if (gdi == 4 & pi == 5) {
      return('P4')
    } else if (gdi == 4 & pi == 6) {
      return('A4')
    } else if (gdi == 5 & pi == 6) {
      return('d5')
    } else if (gdi == 5 & pi == 7) {
      return('P5')
    } else if (gdi == 5 & pi == 8) {
      return('A5')
    } else if (gdi == 6 & pi == 7) {
      return('d6')
    } else if (gdi == 6 & pi == 8) {
      return('m6')
    } else if (gdi == 6 & pi == 9) {
      return('M6')
    } else if (gdi == 6 & pi == 10) {
      return('A6')
    } else if (gdi == 7 & pi == 9) {
      return('d7')
    } else if (gdi == 7 & pi == 10) {
      return('m7')
    } else if (gdi == 7 & pi == 11) {
      return('M7')
    } else if (gdi == 7 & pi == 12) {
      return('A7')
    } else if (gdi == 8 & pi == 11) {
      return('d8')
    } else if (gdi == 8 & pi == 12) {
      return('P8')
    } else if (gdi == 8 & pi == 1) {
      return('A8')
    } else if (gdi == -1 & pi == 1) {
      return('-d1')
    } else if (gdi == -1 & pi == -0) {
      return('-P1')
    } else if (gdi == -1 & pi == -1) {
      return('-A1')
    } else if (gdi == -2 & pi == -0) {
      return('-d2')
    } else if (gdi == -2 & pi == -1) {
      return('-m2')
    } else if (gdi == -2 & pi == -2) {
      return('-M2')
    } else if (gdi == -2 & pi == -3) {
      return('-A2')
    } else if (gdi == -3 & pi == -2) {
      return('-d3')
    } else if (gdi == -3 & pi == -3) {
      return('-m3')
    } else if (gdi == -3 & pi == -4) {
      return('-M3')
    } else if (gdi == -3 & pi == -5) {
      return('-A3')
    } else if (gdi == -4 & pi == -4) {
      return('-d4')
    } else if (gdi == -4 & pi == -5) {
      return('-P4')
    } else if (gdi == -4 & pi == -6) {
      return('-A4')
    } else if (gdi == -5 & pi == -6) {
      return('-d5')
    } else if (gdi == -5 & pi == -7) {
      return('-P5')
    } else if (gdi == -5 & pi == -8) {
      return('-A5')
    } else if (gdi == -6 & pi == -7) {
      return('-d6')
    } else if (gdi == -6 & pi == -8) {
      return('-m6')
    } else if (gdi == -6 & pi == -9) {
      return('-M6')
    } else if (gdi == -6 & pi == -10) {
      return('-A6')
    } else if (gdi == -7 & pi == -9) {
      return('-d7')
    } else if (gdi == -7 & pi == -10) {
      return('-m7')
    } else if (gdi == -7 & pi == -11) {
      return('-M7')
    } else if (gdi == -7 & pi == -12) {
      return('-A7')
    } else if (gdi == -8 & pi == -11) {
      return('-d8')
    } else if (gdi == -8 & pi == -12) {
      return('-P8')
    } else if (gdi == -8 & pi == -1) {
      return('-A8')
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}



### DATA OBJECTS ###
### (IN PROGRESS) ###

# c_flat_major_scale <- c('cb', 'db', 'eb', 'fb', 'gb', 'ab', 'bb')
# c_major_scale <- c('c', 'd', 'e', 'f', 'g', 'a', 'b')
# c_sharp_major_scale <- c('c#', 'd#', 'e#', 'f#', 'g#', 'a#', 'b#')
# d_flat_major_scale <- c()
# d_major_scale <- c()
# e_flat_major_scale <- c()
# e_major_scale <- c()
# f_major_scale <- c()
# f_sharp_major_scale <- c()
# g_flat_major_scale <- c()
# g_major_scale <- c()
# a_flat_major_scale <- c()
# a_major_scale <- c()
# b_flat_major_scale <- c()
# b_major_scale <- c()
# 
# c_flat_natural_minor_scale <- c()
# c_natural_minor_scale <- c()
# c_sharp_natural_minor_scale <- c()
# d_natural_minor_scale <- c()
# e_flat_natural_minor_scale <- c()
# e_natural_minor_scale <- c()
# f_natural_minor_scale <- c()
# f_sharp_natural_minor_scale <- c()
# g_natural_minor_scale <- c()
# g_sharp_natural_minor_scale <- c()
# a_flat_natural_minor_scale <- c()
# a_natural_minor_scale <- c()
# a_sharp_natural_minor_scale <- c()
# b_flat_natural_minor_scale <- c()
# b_natural_minor_scale <- c()
# 
# # melodic ascending
# 
# # harmonic


