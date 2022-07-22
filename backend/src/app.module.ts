import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { AuthModule } from './auth/auth.module';
import { PrismaModule } from './prisma/prisma.module';
import { UsersModule } from './users/users.module';
import { FriendsModule } from './friends/friends.module';
import { BlocksModule } from './blocks/blocks.module';
import { BucketModule } from './bucket/bucket.module';
import { OtpModule } from './otp/otp.module';
import { MailService } from './mail/mail.service';
import { MailModule } from './mail/mail.module';
import { ConfigService } from '@nestjs/config';

@Module({
  imports: [AuthModule, PrismaModule, UsersModule, FriendsModule, BlocksModule, BucketModule, OtpModule, MailModule],
  controllers: [AppController],
  providers: [AppService, MailService, ConfigService],
})
export class AppModule {}
