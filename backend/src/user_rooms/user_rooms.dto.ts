import {IsNotEmpty, IsNumber} from "class-validator";

export class user_roomsDto {
    id: number;

    @IsNotEmpty()
    @IsNumber()
    RoomID: number;

    @IsNotEmpty()
    @IsNumber()
    UserID: number;
}