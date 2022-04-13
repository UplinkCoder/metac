mkdir generated

cl /TP /Iutils utils\gen_metac_match_keyword.c /Os /I.
gen_metac_match_keyword.exe > generated\metac_match_keyword.inl
