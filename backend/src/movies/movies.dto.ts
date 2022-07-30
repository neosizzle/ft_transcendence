import { IsNotEmpty, IsNumber, IsString } from "class-validator";

export class MovieDto {
    id: number;

    @IsNotEmpty()
    @IsString()
    director: string;

    @IsNotEmpty()
    @IsString()
    movieName: string;

    @IsNotEmpty()
    @IsNumber()
    yearReleased: number;
}