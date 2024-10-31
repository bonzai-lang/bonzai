#ifndef WEBSOCKET_H 
#define WEBSOCKET_H

#include <module.h>

Value initWebsocket(Module* module, Value* args, int argc);
Value sendTextFrame(Module* module, Value* args, int argc);
Value receiveFrame(Module* module, Value* args, int argc);
Value closeWebsocket(Module* module, Value* args, int argc);

#endif // WEBSOCKET_H