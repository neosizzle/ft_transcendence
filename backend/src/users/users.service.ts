import { BadRequestException, Injectable, InternalServerErrorException } from '@nestjs/common';
import { Prisma, User, UserStatus } from '@prisma/client';
import { PrismaService } from 'src/prisma/prisma.service';
import { ListObject, ListQuery, validateListquery } from 'src/utils';
import { UserPatchDto } from './dto';

//TODO user delete
// TODO handle bad user status enum
// very ugly solution
/**
 * Transforms string values into values of their specific type 
 * 
 * @param filterBy Filterby string
 * @param filterOn Filteron string
 * @returns 
 */
const transformFilterUser = (filterBy : string, filterOn : string) : string | number | UserStatus | Date =>
{
	if (filterOn === "id" || filterOn === "level")
		return parseFloat(filterBy) ? parseFloat(filterBy) : -1
	if (filterOn === "status")
	{
		switch (filterBy) {
			case "LOGGEDIN":
				return UserStatus.LOGGEDIN;
			case "INGAME":
				return UserStatus.INGAME;
			case "OFFLINE":
				return UserStatus.OFFLINE;
			default:
				return undefined;
		}
	}
	if (filterOn === "createdAt" || filterOn === "updatedAt")
		return new Date(filterBy);
	else
		return filterBy;
}

/**
 * Populates the 'where' option with filter values
 * @param listObj Listobject
 * @returns Generates the sorting field for the query payload
 */
const generateUserWhere = (listObj : ListObject) : Prisma.UserWhereInput => {
	const res : Prisma.UserWhereInput = {};

	if (listObj.operator === "AND")
	{
		res.AND = []
		for (let index = 0; listObj.filterBys && index < listObj.filterBys.length; index++) {
			const filterBy = transformFilterUser(listObj.filterBys[index], listObj.filterOns[index]);
			const filterOn = listObj.filterOns[index];
			const filterEntry = {}
			if ((filterBy == "LOGGEDIN" || filterBy == "INGAME" || filterBy == "OFFLINE") || typeof filterBy !== 'string')
				filterEntry[filterOn] = {equals : filterBy};
			else
				filterEntry[filterOn] = {contains : filterBy};
			res.AND.push(filterEntry)
		}
	}
	else if (listObj.operator === "OR")
	{
		res.OR = []
		for (let index = 0; listObj.filterBys &&  index < listObj.filterBys.length; index++) {
			const filterBy = listObj.filterBys[index];
			const filterOn = listObj.filterOns[index];
			const filterEntry = {}
			if ((filterBy == "LOGGEDIN" || filterBy == "INGAME" || filterBy == "OFFLINE") || typeof filterBy !== 'string')
				filterEntry[filterOn] = {equals : filterBy};
			else
				filterEntry[filterOn] = {contains : filterBy};
			res.OR.push(filterEntry)
		}
	}

	return res;
}

/**
 * Generates pagination settings
 * Generates filters
 * Generates sorts
 * 
 * @param listObj List object
 * @returns Generated find query payload
 */
const generateUserPayload = (listObj : ListObject) : Prisma.UserFindManyArgs => {
	const res : Prisma.UserFindManyArgs = {};
	
	res.take = listObj.pageSize;
	res.skip = (listObj.page - 1) * listObj.pageSize;
	if (listObj.filterBys)
		res.where = generateUserWhere(listObj)
	if (listObj.sortBy)
	{
		res.orderBy = [{}];
		res.orderBy[0][listObj.sortOn] = listObj.sortBy === "Ascending" ? "asc" : "desc"
	}
	return res;
}


@Injectable()
export class UsersService {
	constructor(private prisma : PrismaService){}

	/**
	 * Lists out users
	 * 
	 * @param user User object passed by middleware
	 * @param query query params
	 * @returns result of lisst or error
	 */
	async getUsers(user : User, query : ListQuery)
	{
		// validate query
		const listObj = validateListquery(query, Object.keys(user))
		if (listObj.error)
			throw new BadRequestException(listObj.error)

		// generate and send prisma query
		const payload = generateUserPayload(listObj.data);
		const res = await this.prisma.user.findMany(payload);
		const total_elements = await this.prisma.user.count(<Prisma.UserCountArgs>payload)

		return {data : res, total_elements}
	}

	/**
	 * Updates user properties in db
	 * 
	 * @param user User object from req
	 * @param dto validated request body 
	 * @returns updated user
	 */
	async patchMe(user : User, dto : UserPatchDto)
	{
		try {
			const res = await this.prisma.user.update({
				where : {
					id : user.id
				},
				data: dto 
			})
			return res ;
		} catch (error) {
			console.error(error.code)
			console.error(error.message)
			if (error.code === "P2002")
				throw new BadRequestException("username must be unique")
			throw new InternalServerErrorException("Update failed " + error.message)
		}
	}
}
