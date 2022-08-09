import { Module } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { BlocksService } from "src/users/blocks/blocks.service";
import { PrismaModule } from "src/prisma/prisma.module";
import { PrismaService } from "src/prisma/prisma.service";
import { ChatController } from "./chat.controller";
import { ChatGateway } from "./chat.gateway";
import { ChatService } from "./chat.service";
import { AdminModule } from "../admin/admin.module";
import { BanModule } from "../ban/ban.module";
import { MuteModule } from "../mute/mute.module";
import { MemberModule } from "../member/member.module";
import { RoomModule } from "../room/room.module";
import { RoomService } from "../room/room.service";
import { MemberService } from "../member/member.service";
import { AdminService } from "../admin/admin.service";
@Module({
  imports: [
    PrismaModule,
    AdminModule,
    BanModule,
    MuteModule,
    MemberModule,
    RoomModule,
  ],
  controllers: [ChatController],
  providers: [
    PrismaService,
    ConfigService,
    ChatGateway,
    BlocksService,
    ChatService,
    RoomService,
    MemberService,
    AdminService,
    
  ],
})
export class ChatModule {}
