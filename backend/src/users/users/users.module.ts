import { Module } from "@nestjs/common";
import { ConfigModule } from "@nestjs/config";
import { PrismaService } from "src/prisma/prisma.service";
import { AuthModule } from "../auth/auth.module";
import { BlocksModule } from "../blocks/blocks.module";
import { FriendsModule } from "../friends/friends.module";
import { UsersController } from "./users.controller";
import { UsersGateway } from "./users.gateway";
import { UsersService } from "./users.service";

@Module({
  imports: [ConfigModule, AuthModule, FriendsModule, BlocksModule],
  controllers: [UsersController],
  providers: [UsersService, PrismaService, UsersGateway],
})
export class UsersModule {}
