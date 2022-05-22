__thread void* tlsCtx;

void startThread(void* (*fn)(void*), void* userPointer)
{
    tlsCtx = (void*)1;
}
