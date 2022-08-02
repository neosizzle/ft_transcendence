import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { PrismaModule } from './prisma/prisma.module';
import { ChatModule } from './chat/chat.module';
import { AuthModule } from './users/auth/auth.module';
import { UsersModule } from './users/users/users.module';
import { FriendsModule } from './users/friends/friends.module';
import { BlocksModule } from './users/blocks/blocks.module';
import { BucketModule } from './bucket/bucket.module';
import { OtpModule } from './otp/otp.module';
import { RoomModule } from './room/room.module';
import { MemberModule } from './member/member.module';
import { AdminModule } from './admin/admin.module';
import { BanModule } from './ban/ban.module';
import { MuteModule } from './mute/mute.module';

@Module({
  imports: [AuthModule, PrismaModule, UsersModule, FriendsModule, BlocksModule, BucketModule, OtpModule, ChatModule, RoomModule, MemberModule, AdminModule, BanModule, MuteModule],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
