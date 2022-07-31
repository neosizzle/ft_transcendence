import {IsDateString, IsNotEmpty, IsNumber, IsString } from "class-validator";

export class roomDto {
    RoomID: number;

    @IsNotEmpty()
    @IsString()
    RoomName: string;
}