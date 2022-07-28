import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { AuthModule } from './auth/auth.module';
import { EventsModule } from './events/events.module';
import { PrismaModule } from './prisma/prisma.module';
import { UsersModule } from './users/users.module';
import { FriendsModule } from './friends/friends.module';
import { BlocksModule } from './blocks/blocks.module';
import { BucketModule } from './bucket/bucket.module';
import { MulterModule } from '@nestjs/platform-express';
import { OtpModule } from './otp/otp.module';

@Module({
  imports: [AuthModule, EventsModule, PrismaModule, UsersModule, FriendsModule, BlocksModule, BucketModule, OtpModule],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
