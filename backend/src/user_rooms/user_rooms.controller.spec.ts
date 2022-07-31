import { Test, TestingModule } from '@nestjs/testing';
import { UserRoomsController } from './user_rooms.controller';

describe('UserRoomsController', () => {
  let controller: UserRoomsController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [UserRoomsController],
    }).compile();

    controller = module.get<UserRoomsController>(UserRoomsController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
