import { Module } from "@nestjs/common";
import { ConfigModule } from "@nestjs/config";
import { PrismaService } from "src/prisma/prisma.service";
import { BucketController } from "./bucket.controller";

@Module({
  imports: [ConfigModule],
  controllers: [BucketController],
  providers: [PrismaService],
})
export class BucketModule {}
