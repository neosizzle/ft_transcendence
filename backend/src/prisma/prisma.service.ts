import { Injectable } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { PrismaClient, UserStatus } from '@prisma/client'

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
	
	//clears database for testing
	cleanDb(){
		return this.$transaction([
			this.auth.deleteMany(),
			this.friendship.deleteMany(),
			this.block.deleteMany(),
			this.user.deleteMany(),
		])
	}

	//adds sample user data
	async seedSampleUsers(){
		await this.user.create({data : {
			email : "tes@gmail.com",
			avatar : "asdf.avatar.jpg",
			status : UserStatus.OFFLINE,
			level : 1.12,
			username : "tes",
			intraID : "12345",
			intraName : "tes"
		}})
		await this.user.create({data : {
			email : "tes2t@gmail.com",
			avatar : "asdf2.avatar.jpg",
			status : UserStatus.OFFLINE,
			level : 1.12,
			username : "te4s",
			intraID : "123345",
			intraName : "te4s"
		}})
		await this.user.create({data : {
			email : "test@gmail.com",
			avatar : "asdf.avatar.jpg",
			status : UserStatus.OFFLINE,
			level : 1.12,
			username : "test",
			intraID : "12345",
			intraName : "test"
		}})
		await this.user.create({data : {
			email : "test1@gmail.com",
			avatar : "asdf.avatar.jpg",
			status : UserStatus.OFFLINE,
			level : 1.12,
			username : "test1",
			intraID : "1235",
			intraName : "test1"
		}})
		await this.user.create({data : {
			email : "test2@gmail.com",
			avatar : "asdf.avater.jpg",
			status : UserStatus.OFFLINE,
			level : 1.12,
			username : "test2",
			intraID : "1234a5",
			intraName : "test2"
		}})
	}
}
