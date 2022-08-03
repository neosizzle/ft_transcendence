import { Module } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { MailService } from "src/mail/mail.service";
import { PrismaService } from "src/prisma/prisma.service";
import { AuthController } from "./auth.controller";
import { AuthService } from "./auth.service";

@Module({
  controllers: [AuthController],
  providers: [AuthService, PrismaService, ConfigService, MailService],
})
export class AuthModule {}
