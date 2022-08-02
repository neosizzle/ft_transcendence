import { Injectable } from "@nestjs/common";
import { Chat } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";

import { chatDto } from "./chat.dto";

@Injectable()
export class ChatService {
    constructor(private readonly prismaService: PrismaService) {}
    
    insertChat(dto : chatDto) : Promise<chatDto>{
        return this.prismaService.chat.create({
            data: dto,
        });
    }
}