#define cast_key 0x12520d2

#define CRC32C_QUESTION 0xc372d93b
#define CRC32C_PLUSPLUS 0x61d225eb
#define CRC32C_MINUSMINUS 0x2ebc9331

#ifndef _emptyPointer
#  define _emptyPointer (void*)0x1
#  define emptyNode (metac_node_t) _emptyPointer
#endif


const char* BinExpTypeToChars(metac_binary_expr_kind_t t);

void MetaCParser_PushExprStackBottom(metac_parser_t* self, uint32_t bottom);
void MetaCParser_PopExprStackBottom(metac_parser_t* self);
