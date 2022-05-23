#define KW_PREFIX \
    "tok_kw_"

#define KW_PREFIX_LEN \
    (sizeof(KW_PREFIX) - 1)

#define KW_LEN(KW) \
    ((unsigned int)(sizeof(#KW) - sizeof(KW_PREFIX)))

#define KW_CRC32C(KW) \
    ( crc32c(~0, #KW + KW_PREFIX_LEN, KW_LEN(KW)) )

#define KW_KEY(KW) \
    ( IDENTIFIER_KEY(KW_CRC32C(KW), KW_LEN(KW)) )

#define KW_STR(KW) \
    #KW + KW_PREFIX_LEN

