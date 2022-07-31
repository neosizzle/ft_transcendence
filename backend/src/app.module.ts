import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { PrismaModule } from './prisma/prisma.module';
import { ChatModule } from './chat/chat.module';
import { AuthModule } from './auth/auth.module';
import { UsersModule } from './users/users.module';
import { FriendsModule } from './friends/friends.module';
import { BlocksModule } from './blocks/blocks.module';
import { BucketModule } from './bucket/bucket.module';
import { OtpModule } from './otp/otp.module';
import { RoomModule } from './room/room.module';
import { UserRoomsModule } from './user_rooms/user_rooms.module';

@Module({
  imports: [AuthModule, PrismaModule, UsersModule, FriendsModule, BlocksModule, BucketModule, OtpModule, ChatModule, RoomModule, UserRoomsModule],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
