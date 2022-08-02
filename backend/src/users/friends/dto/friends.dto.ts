import { IsNotEmpty, IsNumber, IsOptional, IsPositive, IsString } from "class-validator";

export class FriendsDto {
	@IsNumber()
	@IsOptional()
	id? : number;

	@IsString()
	@IsOptional()
	intraName? : string;
}

export class FriendsPatchDto {
	@IsNumber()
	@IsPositive()
	@IsNotEmpty()
	friendId : number;

	@IsString()
	@IsNotEmpty()
	reqStatus : string;
}