import { Module } from '@nestjs/common';
import { GameEventsGateway } from './events.gateway';

@Module({
  providers: [GameEventsGateway],
})
export class GameEventsModule {}
