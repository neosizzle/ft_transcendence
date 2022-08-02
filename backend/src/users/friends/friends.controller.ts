import { Body, Controller, Delete, Get, Param, Patch, Post, Query, UseGuards } from '@nestjs/common';
import { User } from '@prisma/client';
import { GetUser } from 'src/users/auth/decorator';
import { AuthGuard } from 'src/users/auth/guard';
import { ListQuery } from 'src/utils';
import { FriendsDto, FriendsPatchDto } from './dto';
import { FriendsService } from './friends.service';

@Controller('/api/v1/friends')
@UseGuards(AuthGuard)
export class FriendsController {
	constructor(private friendsService : FriendsService){}

	// lists out friends of current user
	@Get()
	getFriends(@GetUser() user : User, @Query() query : ListQuery)
	{
		return this.friendsService.getFriends(user, query);
	}

	// adds a new friendship (sends friend request)
	@Post()
	addFriend(@GetUser() user : User, @Body() dto : FriendsDto)
	{
		return this.friendsService.addFriend(user, dto);
	}

	// removes friend (unfriend)
	@Delete(":id")
	deleteFriend(@GetUser() user : User, @Param('id') id : string)
	{
		return this.friendsService.removeFriend(user, parseInt(id, 10));
	}

	// modifies friendship status (accept / reject requests)
	@Patch(":id")
	modifyFriend(@GetUser() user : User, @Param('id') id : string, @Body() dto : FriendsPatchDto)
	{
		return this.friendsService.modifyFriend(user, parseInt(id, 10), dto);
	}
}
