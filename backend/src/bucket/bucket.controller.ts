import { BadRequestException, Controller, Get, NotFoundException, Param, Post, Req, StreamableFile, UploadedFile, UseGuards, UseInterceptors } from '@nestjs/common';
import { FileInterceptor } from '@nestjs/platform-express';
import { Request } from 'express';
import { createReadStream, readdirSync } from 'fs';
import { diskStorage } from 'multer';
import { extname, join } from 'path';
import { AuthGuard } from 'src/users/auth/guard';

interface customReq extends Request
{
	fileError? : string
}

const ONE_MB = 1000000;
const UPLOADS_DIR = "./uploads"

const validFileFilter = (req : customReq, file : Express.Multer.File , callback : (error: Error | null, accept : boolean) => void) => {
	if (!file.originalname.match(/\.(gif|jpe?g|tiff?|png|webp|bmp)$/i))
	{
		req.fileError = "File is not image"
		callback(null, false)
	}
	if (file.size > ONE_MB)
	{
		req.fileError = "File too big";
		callback(null, false)
	}
	return callback(null, true)
}

const generateFileName = (req : Request, file : Express.Multer.File , callback : (error: Error | null, filename: string) => void) => {
	const uniqueSuffix =
	Date.now() + '-' + Math.round(Math.random() * 1e9);
	const ext = extname(file.originalname);
	const filename = `${uniqueSuffix}${ext}`;
	callback(null, filename);
}

@Controller('/api/v1/bucket')
export class BucketController {

	// Upload file
	@UseGuards(AuthGuard)
	@UseInterceptors(FileInterceptor('file', {
		fileFilter : validFileFilter,
		storage : diskStorage({
			destination : UPLOADS_DIR,
			filename : generateFileName
		})
	}))
	@Post()
	uploadFile(
		@Req() req : customReq,
		@UploadedFile() file: Express.Multer.File,
	) {
		if (!file || req.fileError) throw new BadRequestException(req.fileError ? req.fileError : "Bad file");
		return {file : file.filename};
	}

	// get file
	@Get(":id")
	getFile(@Param('id') id : string)
	{
		const dirCont = readdirSync( UPLOADS_DIR );
		const fileName = dirCont.find((elem) => elem === id);
		if (!fileName) throw new NotFoundException()
		const file = createReadStream(join(UPLOADS_DIR, fileName));
		return new StreamableFile(file);
	}
}
