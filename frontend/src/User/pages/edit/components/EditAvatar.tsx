import React, { FunctionComponent, useState } from "react";
import UploadImgModal from "./UploadImgModal";

interface EditAvatarProps {
  currAvatar: string;
  setCurrAvatar: React.Dispatch<React.SetStateAction<string>>;
}

const EditAvatar: FunctionComponent<EditAvatarProps> = ({
  currAvatar,
  setCurrAvatar,
}) => {
  const [openUploadModal, setOpenUploadModal] = useState<boolean>(false);

  return (
    <div>
      {/* Image Button */}
      <img
        onClick={() => setOpenUploadModal(true)}
        className="
				cursor-pointer
				inline-block
				h-20
				w-20
				sm:h-28
				sm:w-28
				rounded-full
				ring-1
				ring-gray
				bg-white
				hover:h-22
				hover:w-22
				sm:hover:h-32
				sm:hover:w-32
				transition-all
				ease-out
				duration-300
				mt-3
				mb-1
				"
        src={currAvatar}
        alt=""
      />

      {/* Upload image modal */}
      {openUploadModal ? (
        <UploadImgModal
          setOpenUploadModal={setOpenUploadModal}
          setCurrAvatar={setCurrAvatar}
        />
      ) : null}
    </div>
  );
};

export default EditAvatar;
