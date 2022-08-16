import { Body, Controller, Get, Post, Query, UseGuards } from "@nestjs/common";
import { AuthGuard } from "src/users/auth/guard";
import { ListQuery } from "src/utils";
import { chatDto } from "./chat.dto";
import { ChatService } from "./chat.service";

@Controller("api/v1/chat")
@UseGuards(AuthGuard)
export class ChatController {
  constructor(
    private readonly chatService: ChatService
  ) {}

  @Get()
  getChat(@Query() query: ListQuery) {
    return this.chatService.getChat(query);
  }
  @Post()
  create(@Body() chatDto: chatDto): Promise<chatDto> {
    return this.chatService.insertChat(chatDto);
  }
}
