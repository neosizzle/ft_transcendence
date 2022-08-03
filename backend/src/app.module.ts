import { Module } from "@nestjs/common";
import { AppController } from "./app.controller";
import { AppService } from "./app.service";
import { PrismaModule } from "./prisma/prisma.module";
import { ChatModule } from "./chat/chat/chat.module";
import { UsersModule } from "./users/users/users.module";
import { BucketModule } from "./bucket/bucket.module";
import { OtpModule } from "./otp/otp.module";
import { MailService } from "./mail/mail.service";
import { MailModule } from "./mail/mail.module";
import { ConfigService } from "@nestjs/config";
import { MuteController } from './chat/mute/mute.controller';
import { MuteModule } from "./chat/mute/mute.module";

@Module({
  imports: [
    PrismaModule,
    BucketModule,
    OtpModule,
    MailModule,
    ChatModule,
    MuteModule,
    UsersModule,
  ],
  controllers: [AppController],
  providers: [AppService, MailService, ConfigService],
})
export class AppModule {}
