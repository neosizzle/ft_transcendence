import { Body, Controller, Get, Patch, Query, UseGuards } from '@nestjs/common';
import { User } from '@prisma/client';
import { GetUser } from 'src/auth/decorator';
import { AuthGuard } from 'src/auth/guard';
import { UserPatchDto } from './dto';
import { UsersService } from "./users.service"
import { ListQuery } from 'src/utils';

@Controller('api/v1/users')
@UseGuards(AuthGuard)
export class UsersController {
	constructor(private usersService : UsersService){}

	// returns current user
	@Get("me")
	getMe(@GetUser() user)
	{
		return user;
	}

	// retreive users list based on filters, sorts and pagination
	@Get()
	getUsers(@GetUser() user: User, @Query() query : ListQuery)
	{
		return this.usersService.getUsers(user, query)
	}

	// edit a user
	@Patch("me")
	patchMe(@GetUser() user: User, @Body() dto : UserPatchDto)
	{
		return this.usersService.patchMe(user, dto);
	}
}
