mkdir generated
cl /TP /I.. utils\gen_metac_match_keyword.c /Os
gen_metac_match_keyword.exe > generated\metac_match_keyword.inl
