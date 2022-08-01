import { Injectable } from "@nestjs/common";
import { PrismaService } from "src/prisma/prisma.service";

import { chatDto } from "./chat.dto";

@Injectable()
export class ChatService {
    constructor(private readonly prismaService: PrismaService) {}
    
    insertChat(dto : chatDto) : Promise<chatDto>{
        dto.timestamp = new Date(dto.timestamp)
        return this.prismaService.chat.create({
            data: dto,
        });
    }
}