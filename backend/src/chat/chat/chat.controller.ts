import { Body, Controller, Get, Post, UseGuards } from "@nestjs/common";
import { Chat } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { AuthGuard } from "src/users/auth/guard";
import { chatDto } from "./chat.dto";
import { ChatService } from "./chat.service";

@Controller("api/v1/chat")
@UseGuards(AuthGuard)
export class ChatController {
  constructor(
    private readonly prismaService: PrismaService,
    private readonly chatService: ChatService
  ) {}

  @Get()
  getChat(): Promise<Chat[]> {
    return this.prismaService.chat.findMany();
  }
  @Post()
  create(@Body() chatDto: chatDto): Promise<chatDto> {
    return this.chatService.insertChat(chatDto);
  }
}
