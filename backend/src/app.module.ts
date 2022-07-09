import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { AuthModule } from './auth/auth.module';
import { PrismaModule } from './prisma/prisma.module';
import { UsersModule } from './users/users.module';
import { FriendsModule } from './friends/friends.module';
import { BlocksModule } from './blocks/blocks.module';

@Module({
  imports: [AuthModule, PrismaModule, UsersModule, FriendsModule, BlocksModule],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
