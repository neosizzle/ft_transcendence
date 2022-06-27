import { Injectable } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { PrismaClient } from '@prisma/client'

@Injectable()
export class PrismaService extends PrismaClient{

	//configures prisma client to connect to which database
	constructor(config : ConfigService)
	{
		super({
			datasources : {
				db: {
					url : config.get('DATABASE_URL')//env here
				}
			}
		})
	}	
}