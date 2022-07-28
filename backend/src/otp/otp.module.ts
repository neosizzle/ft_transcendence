import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { MailService } from 'src/mail/mail.service';
import { PrismaService } from 'src/prisma/prisma.service';
import { OtpController } from './otp.controller';
import { OtpService } from './otp.service';

@Module({
  imports: [ConfigModule],
  controllers: [OtpController],
  providers: [OtpService, PrismaService, MailService]
})
export class OtpModule {}
