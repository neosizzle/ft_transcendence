import { Module } from '@nestjs/common';
import { PrismaModule } from 'src/prisma/prisma.module';
import { MoviesController } from './movies.controller';

@Module({
  imports: [PrismaModule],
  controllers: [MoviesController]
})
export class MoviesModule {}
