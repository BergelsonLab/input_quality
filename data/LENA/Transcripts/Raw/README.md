# input_quality/data/LENA/Transcripts/Raw/

This folder contains raw data files for the LENA transcripts, exported from LENA.

## Files
  - VI_LENA_and_TD_matches_2023-05-09.csv: A CSV file containing the matches between the blind participants (VI) and their sighted matches (TD) for the LENA recordings.
    - begintime_msec: Start time of the segment in milliseconds.
    - endtime_msec: End time of the segment in milliseconds.
    - duration_msec: Duration of the segment in milliseconds.
    - vcm: Vocalization maturity category.
    - context: is this segment just context for a coded region?
    - sampling_type: whether this is a random sample or a high-volume sample
    - xds: addressee
    - PI: whether segment contains private or identifiable information (ELAN_wrangle.R should have already filtered out any segments with PI though)
    - code: whether this region should be coded
    - lex: Lexical code.
    - mwu: whether CHI utterance is multi-word
    - is_silent: Indicator for silent segments.
    - code_num: number assigned to each coded region in the file
    - File: File name.
    - File Path: File path.
    - speaker: Speaker identifier.
    - utterance: Utterance text.
    - VIHI_ID: Unique identifier for each participant.
    - group: Group of the participant (e.g., blind = VI, sighted = TD).

Maintained by: Erin Campbell (erin.e.campbell@duke.edu)
Last updated: 5/29/2023